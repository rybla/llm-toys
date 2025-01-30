-- Defines the low-level interface to generation using an LLM.
module Ai.Llm.Core where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Literal (Literal)
import Data.Literal as Literal
import Data.Newtype (class Newtype, unwrap)
import Data.Optional (Optional)
import Data.PartialRecord (PartialRecord)
import Data.TaggedUnion (TaggedUnion)
import Data.Variant (Variant, case_)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Foreign.Object (Object)
import Utility (inj, on)

--------------------------------------------------------------------------------
-- generate
--------------------------------------------------------------------------------

generate
  :: PartialRecord
       ( apiKey :: String
       , baseURL :: Optional String
       , model :: String
       , messages :: Array Message
       , tools :: Optional (Array Tool)
       , tool_choice :: Optional ToolChoice
       )
  -> Aff (Either String AssistantMessageValue)
generate args = do
  result <- generate_ { ok: pure, err: throwError } (args # encodeJson) # toAffE
  case result of
    Left err -> pure (throwError err)
    Right json_msg -> do
      case decodeJson @AssistantMessageValue json_msg of
        Left err -> pure (throwError (printJsonDecodeError err))
        Right msg -> pure (pure msg)

foreign import generate_
  :: { ok :: Json -> Either String Json, err :: String -> Either String Json }
  -> Json
  -> Effect (Promise (Either String Json))

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type Message = TaggedUnion "role"
  ( system :: PartialRecord (name :: Optional String, content :: String)
  , user :: PartialRecord (name :: Optional String, content :: String)
  , assistant :: AssistantMessageValue
  , tool :: PartialRecord (name :: Optional String, tool_call_id :: String, content :: String)
  )

type AssistantMessage = TaggedUnion "role"
  (assistant :: AssistantMessageValue)

type AssistantMessageValue =
  PartialRecord (content :: Optional String, tool_calls :: Optional (Array ToolCall))

data ToolCall = ToolCall { id :: String, function :: { name :: String, arguments :: String } }

instance EncodeJson ToolCall where
  encodeJson (ToolCall x) = encodeJson
    { "type": "function"
    , id: x.id
    , function: x.function
    }

instance DecodeJson ToolCall where
  decodeJson json = do
    x <- decodeJson @{ "type" :: Literal ("function" :: Unit), id :: String, function :: { name :: String, arguments :: String } } json
    pure $ ToolCall { id: x.id, function: x.function }

type Tool = TaggedUnion "type"
  ( function :: FunctionDefinition
  )

type FunctionDefinition =
  { name :: String
  , description :: String
  , parameters :: FunctionParameters
  , strict :: Boolean
  }

type FunctionParameters = Object Json

newtype ToolChoice = ToolChoice
  ( Variant
      ( none :: Unit
      , auto :: Unit
      , required :: Unit
      , named :: String
      )
  )

derive instance Newtype ToolChoice _

noneToolChoice = ToolChoice $ inj @"none" unit :: ToolChoice
autoToolChoice = ToolChoice $ inj @"auto" unit :: ToolChoice
requiredToolChoice = ToolChoice $ inj @"required" unit :: ToolChoice

namedToolChoice :: String -> ToolChoice
namedToolChoice str = ToolChoice $ inj @"named" str

type NamedToolChoice = { "type" :: Literal ("function" :: Unit), function :: { name :: String } }

instance EncodeJson ToolChoice where
  encodeJson = unwrap >>>
    ( case_
        # on @"none" (\_ -> encodeJson "none")
        # on @"auto" (\_ -> encodeJson "auto")
        # on @"required" (\_ -> encodeJson "required")
        # on @"named" (\name -> encodeJson @NamedToolChoice { "type": Literal.make @"function", function: { name } })
    )

instance DecodeJson ToolChoice where
  decodeJson json = case decodeJson @String json of
    Right "none" -> pure $ ToolChoice $ inj @"none" unit
    Right "auto" -> pure $ ToolChoice $ inj @"auto" unit
    Right "required" -> pure $ ToolChoice $ inj @"required" unit
    Right str -> throwError $ TypeMismatch $ "invalid ToolChoice: " <> show str
    Left _ -> case decodeJson @NamedToolChoice json of
      Right x -> pure $ ToolChoice $ inj @"named" x.function.name
      Left _ -> throwError $ TypeMismatch $ "invalid ToolChoice"

