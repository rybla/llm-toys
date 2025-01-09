module Ai.Llm where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, printJsonDecodeError)
import Data.Either (Either(..))
import Data.Optional (Optional)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Partial.Unsafe (unsafeCrashWith)

generate
  :: { apiKey :: String
     , baseURL :: String
     , model :: String
     , messages :: Array Message
     , tools :: Optional (Array Tool)
     , tool_choice :: Optional ToolChoice
     }
  -> Aff (Either String Message)
generate { apiKey, baseURL, model, messages, tools, tool_choice } = do
  result <-
    generate_
      { ok: pure
      , err: throwError
      , apiKey
      , baseURL
      , model
      , messages: messages # map encodeJson
      , tools: tools # map (map encodeJson)
      , tool_choice: tool_choice # map encodeJson
      } # toAffE
  case result of
    Left err -> pure (throwError err)
    Right json_msg -> do
      case decodeJson @Message json_msg of
        Left err -> pure (throwError (printJsonDecodeError err))
        Right msg -> pure (pure msg)

foreign import generate_
  :: { ok :: Json -> Either String Json
     , err :: String -> Either String Json
     , apiKey :: String
     , baseURL :: String
     , model :: String
     , messages :: Array Json
     , tools :: Optional (Array Json)
     , tool_choice :: Optional Json
     }
  -> Effect (Promise (Either String Json))

data Message
  = SystemMessage { name :: Optional String, content :: String }
  | UserMessage { name :: Optional String, content :: String }
  | AssistantMessage { name :: Optional String, content :: String, tool_calls :: ToolCall }
  | ToolMessage { tool_call_id :: String, content :: String }

instance EncodeJson Message where
  encodeJson = unsafeCrashWith "TODO"

instance DecodeJson Message where
  decodeJson = unsafeCrashWith "TODO"

data ToolCall

instance EncodeJson ToolCall where
  encodeJson = unsafeCrashWith "TODO"

instance DecodeJson ToolCall where
  decodeJson = unsafeCrashWith "TODO"

data Tool

instance EncodeJson Tool where
  encodeJson = unsafeCrashWith "TODO"

instance DecodeJson Tool where
  decodeJson = unsafeCrashWith "TODO"

data ToolChoice

instance EncodeJson ToolChoice where
  encodeJson = unsafeCrashWith "TODO"

instance DecodeJson ToolChoice where
  decodeJson = unsafeCrashWith "TODO"

