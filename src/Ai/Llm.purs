-- Defines the high-level interface to generation using an LLM.
module Ai.Llm where

import Prelude

import Ai.Llm.Core as Core
import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), encodeJson, printJsonDecodeError)
import Data.Argonaut as Argonaut
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Optional (Optional)
import Data.Optional as Optional
import Data.PartialRecord (PartialRecord(..))
import Data.Traversable (traverse)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Foreign.Object (Object)
import Foreign.Object as Object
import Utility (inj)

--------------------------------------------------------------------------------
-- generate
--------------------------------------------------------------------------------

generate
  :: { apiKey :: String
     , baseUrl :: String
     , model :: String
     , messages :: Array Message
     , tools :: Array Tool
     , tool_choice :: ToolChoice
     }
  -> Aff (String \/ AssistantMessage)
generate args = do
  result <- generate_ { ok: pure, err: throwError } (args # encodeJson) # toAffE
  case result of
    Left err -> pure $ throwError err
    Right json_msg -> do
      case decodeJson_Message_assistant json_msg of
        Left err -> pure $ throwError $ printJsonDecodeError err
        Right msg -> pure $ pure msg

foreign import generate_
  :: { ok :: Json -> Either String Json, err :: String -> Either String Json }
  -> Json
  -> Effect (Promise (Either String Json))

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type AssistantMessage = { content :: Maybe String, tool_calls :: Array ToolCall }

newtype Message = Message
  ( Variant
      ( system :: { name :: Maybe String, content :: String }
      , user :: { name :: Maybe String, content :: String }
      , assistant :: AssistantMessage
      , tool ::
          { name :: String
          , tool_call_id :: String
          -- output of tool
          , content :: String
          }
      )
  )

derive instance Newtype Message _

instance EncodeJson Message where
  encodeJson = unwrap >>> match
    { system: \msg -> encodeJson $ PartialRecord { "role": "system", name: msg.name # Optional.fromMaybe, content: msg.content }
    , user: \msg -> encodeJson $ PartialRecord { "role": "user", name: msg.name # Optional.fromMaybe, content: msg.content }
    , assistant: \msg -> encodeJson $ PartialRecord { "role": "assistant", content: msg.content }
    , tool: \msg -> encodeJson $ PartialRecord { "role": "tool", name: msg.name, tool_call_id: msg.tool_call_id, content: msg.content }
    }

instance DecodeJson Message where
  decodeJson json | Right (PartialRecord { role: "system", name, content }) <- decodeJson @(PartialRecord ("role" :: String, "name" :: Optional String, content :: String)) json = do
    pure $ wrap $ inj @"system" { name: name # Optional.toMaybe, content }
  decodeJson json | Right (PartialRecord { role: "user", name, content }) <- decodeJson @(PartialRecord ("role" :: String, "name" :: Optional String, content :: String)) json = do
    pure $ wrap $ inj @"user" { name: name # Optional.toMaybe, content }
  decodeJson json | Right (PartialRecord { role: "assistant", content, tool_calls }) <- decodeJson @(PartialRecord ("role" :: String, content :: Optional String, tool_calls :: Json)) json = do
    tool_calls' <- tool_calls # decodeJson
    pure $ wrap $ inj @"assistant" { content: content # Optional.toMaybe, tool_calls: tool_calls' }
  decodeJson json | Right (PartialRecord { name, role: "tool", tool_call_id, content }) <- decodeJson @(PartialRecord ("role" :: String, name :: String, tool_call_id :: String, content :: String)) json = do
    pure $ wrap $ inj @"tool" { name, tool_call_id, content }
  decodeJson json = throwError $ UnexpectedValue json

decodeJson_Message_assistant :: Json -> JsonDecodeError \/ AssistantMessage
decodeJson_Message_assistant json | Right (PartialRecord { role: "assistant", content, tool_calls }) <- decodeJson @(PartialRecord ("role" :: String, content :: Optional String, tool_calls :: Json)) json = do
  tool_calls' <- tool_calls # decodeJson
  pure { content: content # Optional.toMaybe, tool_calls: tool_calls' }
decodeJson_Message_assistant json = throwError $ UnexpectedValue json

newtype ToolCall = ToolCall
  { id :: String
  , function ::
      { name :: String
      -- `arguments` should be parseable into a JSON that decodes to the named tool required arguments
      , arguments :: String
      }
  }

derive instance Newtype ToolCall _

instance EncodeJson ToolCall where
  encodeJson = unwrap >>> encodeJson

instance DecodeJson ToolCall where
  decodeJson = decodeJson >>> map wrap

newtype Tool = Tool
  ( Variant
      ( function :: FunctionDefinition
      )
  )

derive instance Newtype Tool _

instance EncodeJson Tool where
  encodeJson = unwrap >>> match
    { function: \tool -> encodeJson { "type": "function", function: tool } }

instance DecodeJson Tool where
  decodeJson json | Right { "type": "function", function } <- decodeJson @{ "type" :: String, function :: Json } json = do
    function' <- json # decodeJson @FunctionDefinition
    pure $ wrap $ inj @"function" function'
  decodeJson json = throwError $ UnexpectedValue json

type FunctionDefinition =
  { name :: String
  , description :: String
  , parameters :: FunctionParameters
  }

newtype FunctionParameters = FunctionParameters (Map String FunctionParameter)

derive instance Newtype FunctionParameters _

instance EncodeJson FunctionParameters where
  encodeJson = unwrap >>> map encodeJson >>> fromJsonMapToJsonObject

instance DecodeJson FunctionParameters where
  decodeJson json | Right parameters <- decodeJson @(Object Json) json = do
    parameters' <- parameters # traverse decodeJson
    pure $ FunctionParameters $ Map.fromFoldable $ (Object.toUnfoldable parameters' :: List _)
  decodeJson json = throwError $ UnexpectedValue json

newtype FunctionParameter = FunctionParameter
  ( Variant
      ( object :: FunctionParameters
      , string :: { description :: String }
      , number :: { description :: String }
      )
  )

derive instance Newtype FunctionParameter _

instance EncodeJson FunctionParameter where
  encodeJson = unwrap >>> match
    { object: \parameters -> encodeJson { "type": "object", "parameters": parameters # encodeJson }
    , string: \{ description } -> encodeJson { "type": "string", description }
    , number: \{ description } -> encodeJson { "type": "number", description }
    }

fromJsonMapToJsonObject :: Map String Json -> Json
fromJsonMapToJsonObject m = (m # Map.toUnfoldable :: List _) # Object.fromFoldable # Argonaut.fromObject

instance DecodeJson FunctionParameter where
  decodeJson json | Right { "type": "object", parameters } <- decodeJson @{ "type" :: String, "parameters" :: Json } json = do
    parameters' <- parameters # decodeJson
    pure $ wrap $ inj @"object" $ parameters'
  decodeJson json | Right { "type": "string", description } <- decodeJson @{ "type" :: String, "description" :: String } json = pure $ wrap $ inj @"string" { description }
  decodeJson json | Right { "type": "number", description } <- decodeJson @{ "type" :: String, "description" :: String } json = pure $ wrap $ inj @"number" { description }
  decodeJson json = throwError $ UnexpectedValue json

newtype ToolChoice = ToolChoice
  ( Variant
      ( none :: Unit
      , auto :: Unit
      , required :: Unit
      , named :: String
      )
  )

derive instance Newtype ToolChoice _

instance EncodeJson ToolChoice where
  encodeJson = unwrap >>> match
    { none: \_ -> "none" # encodeJson
    , auto: \_ -> "auto" # encodeJson
    , required: \_ -> "required" # encodeJson
    , named: \name -> { "type": "function", "function": { "name": name } } # encodeJson
    }

instance DecodeJson ToolChoice where
  decodeJson json | Right "none" <- decodeJson @String json = pure $ wrap $ inj @"none" unit
  decodeJson json | Right "auto" <- decodeJson @String json = pure $ wrap $ inj @"auto" unit
  decodeJson json | Right "required" <- decodeJson @String json = pure $ wrap $ inj @"required" unit
  decodeJson json | Right { "type": "function", "function": { "name": name } } <- decodeJson @{ "type" :: String, "function" :: { "name" :: String } } json = pure $ wrap $ inj @"named" name
  decodeJson json = throwError $ UnexpectedValue json

