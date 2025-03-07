module Ai2.Llm
  ( Config
  , Msg(..)
  , SystemMsg
  , mkSystemMsg
  , UserMsg
  , mkUserMsg
  , AssistantMsg(..)
  , TextAssistantMsg
  , mkTextAssistantMsg
  , StructureAssistantMsg
  , mkStructureAssistantMsg
  , ToolAssistantMsg
  , mkToolAssistantMsg
  , ToolMsg
  , ToolCall
  , Tool(..)
  , FunctionTool
  , ToolChoice(..)
  -- generate functions
  , generate
  , generate_tools
  , generate_structure
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, parseJson)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.JsonSchema (class DecodeJsonFromSchema, class ToJsonSchema, decodeJsonFromSchema, toJsonSchema)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Utility (format)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

-- Config

type Config =
  { baseURL :: String
  , model :: String
  , apiKey :: String
  }

-- Msg

data Msg
  = SystemMsg SystemMsg
  | UserMsg UserMsg
  | AssistantMsg AssistantMsg
  | ToolMsg ToolMsg

instance EncodeJson Msg where
  encodeJson (SystemMsg msg) = encodeJson { role: "system", content: msg.content }
  encodeJson (UserMsg msg) = encodeJson { role: "user", content: msg.content }
  encodeJson (AssistantMsg msg) = encodeJson msg
  encodeJson (ToolMsg msg) = encodeJson { role: "tool", name: msg.name, tool_call_id: msg.tool_call_id, content: msg.content }

type SystemMsg = { content :: String }

mkSystemMsg content = SystemMsg { content }

type UserMsg = { content :: String }
mkUserMsg content = UserMsg { content }

data AssistantMsg
  = TextAssistantMsg TextAssistantMsg
  | ToolAssistantMsg ToolAssistantMsg
  | StructureAssistantMsg StructureAssistantMsg

instance EncodeJson AssistantMsg where
  encodeJson (TextAssistantMsg msg) = encodeJson { role: "assistant", content: msg.content }
  encodeJson (ToolAssistantMsg msg) = encodeJson { role: "assistant", content: msg.content, tool_calls: msg.toolCalls }
  encodeJson (StructureAssistantMsg msg) = encodeJson { role: "assistant", content: msg.parsed # stringify, parsed: msg.parsed }

type TextAssistantMsg = { content :: String }

mkTextAssistantMsg content = AssistantMsg $ TextAssistantMsg { content }

type StructureAssistantMsg = { parsed :: Json }

mkStructureAssistantMsg parsed = AssistantMsg $ StructureAssistantMsg { parsed }

type ToolAssistantMsg = { content :: Maybe String, toolCalls :: Array ToolCall }

mkToolAssistantMsg content toolCalls = AssistantMsg $ ToolAssistantMsg { content, toolCalls }

data ToolCall =
  FunctionToolCall { id :: String, name :: String, args :: Json }

instance Show ToolCall where
  show (FunctionToolCall { id, name, args }) = "{ id: {{id}}, name: {{name}}, args: {{args}} }" # format { id, name, args: stringify args }

instance EncodeJson ToolCall where
  encodeJson (FunctionToolCall toolCall) = encodeJson
    { id: toolCall.id
    , type: "function"
    , function:
        { name: toolCall.name
        , arguments: stringify toolCall.args
        }
    }

instance DecodeJson ToolCall where
  decodeJson json | Right toolCall <- json # decodeJson @{ id :: String, function :: { name :: String, arguments :: Json } } = Right $ FunctionToolCall
    { id: toolCall.id, name: toolCall.function.name, args: toolCall.function.arguments }
  decodeJson json = Left $ UnexpectedValue json

type ToolMsg = { name :: String, tool_call_id :: String, content :: String }

-- Tool

-- following example of JsonSchema, make these typed somehow in the interface 
-- instead of an array of tools given to generate_tools, instead probably need to give a record where each tool specifies its return type in a typed way
data Tool = FunctionTool FunctionTool

instance EncodeJson Tool where
  encodeJson (FunctionTool function) = encodeJson
    { type: "function"
    , function:
        { name: function.name
        , description: function.description
        , parameters: function.parameters
        , strict: true
        }
    }

type FunctionTool =
  { name :: String
  , description :: String
  , parameters :: Json
  }

-- ToolChoice

data ToolChoice
  = NoneToolChoice
  | AutoToolChoice
  | RequiredToolChoice
  | NamedToolChoice String

--------------------------------------------------------------------------------
-- endpoints
-- TODO: handle failure cases
--------------------------------------------------------------------------------

foreign import generate_
  :: forall a
   . { error :: String -> a, ok :: Json -> a }
  -> Json
  -> Effect (Promise a)

generate :: { config :: Config, messages :: Array Msg } -> Aff (String \/ TextAssistantMsg)
generate args =
  ( toAffE $ generate_ { error: Left, ok: Right } $ encodeJson
      { baseURL: args.config.baseURL
      , model: args.config.model
      , apiKey: args.config.apiKey
      , messages: args.messages
      }
  ) <#> case _ of
    Left err -> Left $ "generate: " <> err
    Right response -> case response # decodeJson @{ content :: String } of
      Right { content } -> Right { content }
      Left err -> Left $ "generate: " <> printJsonDecodeError err

generate_tools :: { config :: Config, tools :: Array Tool, messages :: Array Msg } -> Aff (String \/ (TextAssistantMsg \/ ToolAssistantMsg))
generate_tools args =
  ( toAffE $ generate_ { error: Left, ok: Right } $ encodeJson
      { baseURL: args.config.baseURL
      , model: args.config.model
      , apiKey: args.config.apiKey
      , tools: args.tools
      , tool_choice: "auto"
      , messages: args.messages
      }
  ) <#> case _ of
    Left err -> Left $ "generate_tools: " <> err
    Right response -> case response # decodeJson @{ content :: String, tool_calls :: Array ToolCall } of
      Right { content, tool_calls } -> Right $ Right { content: Just content, toolCalls: tool_calls }
      Left err1 -> case response # decodeJson @{ tool_calls :: Array ToolCall } of
        Right { tool_calls } -> Right $ Right { content: Nothing, toolCalls: tool_calls }
        Left err2 -> case response # decodeJson @{ content :: String } of
          Right { content } -> Right $ Left { content }
          Left err3 -> Left $ "generate_tools:\n" <> Array.intercalate "\n" ([ err1, err2, err3 ] # map printJsonDecodeError)

generate_structure
  :: forall @r
   . ToJsonSchema (Record r)
  => DecodeJsonFromSchema (Record r)
  => { config :: Config, name :: String, messages :: Array Msg }
  -> Aff (String \/ Record r)
generate_structure args = do
  -- Console.log $ stringifyWithIndent 4 $ encodeJson $
  --   { type: "json_schema"
  --   , json_schema:
  --       { name: args.name
  --       , strict: true
  --       , schema: toJsonSchema @(Record r)
  --       }
  --   }
  ( toAffE $ generate_ { error: Left, ok: Right } $ encodeJson
      { baseURL: args.config.baseURL
      , model: args.config.model
      , apiKey: args.config.apiKey
      , messages: args.messages
      , response_format: encodeJson $
          { type: "json_schema"
          , json_schema:
              { name: args.name
              , strict: true
              , schema: toJsonSchema @(Record r)
              }
          }
      }
  ) <#> case _ of
    Left err -> Left $ "generate_structure: " <> err
    Right response -> case response # decodeJson @{ content :: String } of
      Right { content } -> case parseJson content of
        Left err -> Left $ printJsonDecodeError err
        Right parsed -> case parsed # decodeJsonFromSchema of
          Left err -> Left $ printJsonDecodeError err
          Right a -> pure a
      Left err -> Left $ "generate_structure: failed to parsed content as JSON: " <> printJsonDecodeError err

