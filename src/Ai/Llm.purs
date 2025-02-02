-- TODO: test this out
-- Defines the high-level interface to generation using an LLM.
module Ai.Llm where

import Prelude

import Ai.Llm.Core as Core
import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), encodeJson, printJsonDecodeError, stringifyWithIndent)
import Data.Argonaut as Argonaut
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Array.NonEmpty as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Optional (Optional)
import Data.Optional as Optional
import Data.PartialRecord (PartialRecord(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Unfoldable (none)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Foreign.Object (Object)
import Foreign.Object as Object
import Utility (inj)

--------------------------------------------------------------------------------
-- generate
--------------------------------------------------------------------------------

type GenerateConfig =
  { apiKey :: String
  , baseURL :: String
  , model :: String
  }

generate_basic
  :: { config :: GenerateConfig
     , messages :: Array Message
     }
  -> Aff String
generate_basic { config: { apiKey, baseURL, model }, messages } =
  generate_raw (encodeJson { apiKey, baseURL, model, messages }) >>= case _ of
    { content: Nothing } -> throwError $ Aff.error $ "no content"
    { content: Just content } -> pure content

generate_with_tools
  :: { config :: GenerateConfig
     , messages :: Array Message
     , tools :: Array Tool
     , tool_choice :: ToolChoice
     }
  -> Aff
       { content :: Maybe String
       , tool_calls :: Maybe (Array ToolCall)
       }
generate_with_tools { config: { apiKey, baseURL, model }, messages, tools, tool_choice } =
  generate_raw $ encodeJson
    { apiKey, baseURL, model, messages, tools, tool_choice }

generate_with_format
  :: { config :: GenerateConfig
     , messages :: Array Message
     , format :: Structure
     }
  -> Aff String
generate_with_format { config: { apiKey, baseURL, model }, messages, format } = do
  generate_raw (encodeJson { apiKey, baseURL, model, messages, format }) >>= case _ of
    { content: Nothing } -> throwError $ Aff.error $ "no content"
    { content: Just content } -> pure content

--------------------------------------------------------------------------------
-- generate_raw
--------------------------------------------------------------------------------

generate_raw
  :: Json
  -> Aff
       { content :: Maybe String
       , tool_calls :: Maybe (Array ToolCall)
       }
generate_raw args = do
  Console.log $ "[generate input]\n" <> stringifyWithIndent 4 args
  result <- generate_ { ok: pure, err: throwError } args # toAffE
  case result of
    Left err -> throwError $ Aff.error err
    Right json_msg -> do
      Console.log $ "[generate output]\n" <> stringifyWithIndent 4 json_msg
      case decodeJson_Message_assistant json_msg of
        Left err -> throwError $ Aff.error $ printJsonDecodeError err
        Right msg -> pure msg

foreign import generate_
  :: { ok :: Json -> Either String Json, err :: String -> Either String Json }
  -> Json
  -> Effect (Promise (Either String Json))

-- --------------------------------------------------------------------------------
-- -- generate refinements
-- --------------------------------------------------------------------------------

-- generate_without_tools
--   :: { config ::
--          { apiKey :: String
--          , baseURL :: String
--          , model :: String
--          }
--      , messages :: Array Message
--      }
--   -> Aff String
-- generate_without_tools { config, messages } =
--   generate_ { config, messages } >>= case _ of
--     { tool_calls } | not $ null tool_calls -> throwError $ Aff.error $ "generation error: shouldn't be using tools: " <> show tool_calls
--     { content: Nothing } -> throwError $ Aff.error $ "generation error: no content"
--     { content: Just reply } -> pure reply

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

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

type AssistantMessage = { content :: Maybe String, tool_calls :: Maybe (Array ToolCall) }

mkSystemMessage :: String -> Message
mkSystemMessage content = inj @"system" { name: none, content } # wrap

mkUserMessage :: String -> Message
mkUserMessage content = inj @"user" { name: none, content } # wrap

mkAssistantMessage :: String -> Message
mkAssistantMessage content = inj @"assistant" { content: content # pure, tool_calls: none } # wrap

mkToolMessage :: String -> String -> String -> Message
mkToolMessage name tool_call_id content = inj @"tool" { name, tool_call_id, content } # wrap

derive instance Newtype Message _
derive newtype instance Show Message

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
decodeJson_Message_assistant json | Right (PartialRecord { role: "assistant", content, tool_calls }) <- decodeJson @(PartialRecord ("role" :: String, content :: Optional Json, tool_calls :: Optional Json)) json = do
  content' <- content # traverse decodeJson
  tool_calls' <- tool_calls # traverse decodeJson
  pure { content: content' # Optional.toMaybe, tool_calls: tool_calls' # Optional.toMaybe }
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
derive newtype instance Show ToolCall

instance EncodeJson ToolCall where
  encodeJson = unwrap >>> encodeJson

instance DecodeJson ToolCall where
  decodeJson = decodeJson >>> map wrap

-- TODO: i know this form looks right, but no, its just annoying to deal with other places
-- newtype Tool = Tool
--   ( Variant
--       ( function :: FunctionDefinition
--       )
--   )
newtype Tool = Tool FunctionDefinition

derive instance Newtype Tool _
derive newtype instance Show Tool

instance EncodeJson Tool where
  encodeJson = unwrap >>> \tool -> encodeJson { "type": "function", function: tool }

instance DecodeJson Tool where
  decodeJson json | Right { "type": "function", function } <- decodeJson @{ "type" :: String, function :: Json } json = do
    function' <- json # decodeJson @FunctionDefinition
    pure $ wrap function'
  decodeJson json = throwError $ UnexpectedValue json

newtype FunctionDefinition = FunctionDefinition
  { name :: String
  , description :: String
  -- , parameters :: StructureFields
  , parameters :: Structure
  }

derive newtype instance Show FunctionDefinition

derive instance Newtype FunctionDefinition _

instance EncodeJson FunctionDefinition where
  encodeJson (FunctionDefinition def) = encodeJson
    { "name": def.name
    , "description": def.description
    -- , "parameters": Structure $ inj @"object" def.parameters
    , "parameters": def.parameters
    , "strict": true
    }

instance DecodeJson FunctionDefinition where
  decodeJson json | Right x <- decodeJson @{ name :: Json, description :: Json, parameters :: Json, required :: Json } json = do
    name <- x.name # decodeJson
    description <- x.description # decodeJson
    parameters <- x.parameters # decodeJson
    pure $ FunctionDefinition { name, description, parameters }
  decodeJson json = throwError $ UnexpectedValue json

newtype StructureFields = StructureFields (Map String Structure)

derive instance Newtype StructureFields _
derive instance Generic StructureFields _

instance Show StructureFields where
  show x = genericShow x

instance EncodeJson StructureFields where
  encodeJson = unwrap >>> map encodeJson >>> fromMapJsonToObjectJson

{-
instance EncodeJson StructureFields where
  encodeJson (StructureFields m) =
    m
      # map encodeJson
      # Map.insert "required" (m # Map.keys # Array.fromFoldable # encodeJson)
      # fromMapJsonToObjectJson
-}

instance DecodeJson StructureFields where
  decodeJson json | Right parameters <- decodeJson @(Object Json) json = do
    parameters' <- parameters # traverse decodeJson
    pure $ StructureFields $ Map.fromFoldable $ (Object.toUnfoldable parameters' :: List _)
  decodeJson json = throwError $ UnexpectedValue json

newtype Structure = Structure
  ( Variant
      ( object :: StructureFields
      , string :: { description :: String }
      , number :: { description :: String }
      )
  )

derive instance Newtype Structure _
derive newtype instance Show Structure

instance EncodeJson Structure where
  encodeJson = unwrap >>> match
    { object: \properties -> encodeJson
        { "type": "object"
        , "properties": properties # encodeJson
        , "required": properties # unwrap # Map.keys # Array.fromFoldable
        }
    , string: \{ description } -> encodeJson { "type": "string", description }
    , number: \{ description } -> encodeJson { "type": "number", description }
    }

fromMapJsonToObjectJson :: Map String Json -> Json
fromMapJsonToObjectJson m = (m # Map.toUnfoldable :: List _) # Object.fromFoldable # Argonaut.fromObject

instance DecodeJson Structure where
  decodeJson json | Right { "type": "object", properties } <- decodeJson @{ "type" :: String, "properties" :: Json } json = do
    properties' <- properties # decodeJson
    pure $ wrap $ inj @"object" $ properties'
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
derive newtype instance Show ToolChoice

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

