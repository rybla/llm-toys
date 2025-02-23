module Ai2.Llm
  ( Config
  , Msg(..)
  , SystemMsg
  , UserMsg
  , AssistantMsg
  , ToolMsg
  , TextAssistantMsg
  , StructureAssistantMsg
  , ToolAssistantMsg
  , ToolCall
  , Tool(..)
  , FunctionTool
  , ToolChoice(..)
  , SchemaDef(..)
  , Schema(..)
  , generate
  , generate_tool
  , generate_structure
  ) where

import Prelude

import Control.Category (identity)
import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, parseJson, printJsonDecodeError, stringify)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..), either, fromRight')
import Data.Either.Nested (type (\/))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Foreign.Object (Object)
import Foreign.Object as Object
import Utility (bug)

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

type UserMsg = { content :: String }

data AssistantMsg
  = TextAssistantMsg TextAssistantMsg
  | ToolAssistantMsg ToolAssistantMsg
  | StructureAssistantMsg StructureAssistantMsg

instance EncodeJson AssistantMsg where
  encodeJson (TextAssistantMsg msg) = encodeJson { role: "assistant", content: msg.content }
  encodeJson (ToolAssistantMsg msg) = encodeJson { role: "assistant", content: msg.content, tool_calls: msg.toolCalls }
  encodeJson (StructureAssistantMsg msg) = encodeJson { role: "assistant", content: msg.parsed # stringify, parsed: msg.parsed }

type TextAssistantMsg = { content :: String }

type StructureAssistantMsg = { parsed :: Json }

type ToolAssistantMsg = { content :: Maybe String, toolCalls :: Array ToolCall }

data ToolCall =
  FunctionToolCall { id :: String, name :: String, args :: Json }

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
  , parameters :: Schema
  }

-- ToolChoice

data ToolChoice
  = NoneToolChoice
  | AutoToolChoice
  | RequiredToolChoice
  | NamedToolChoice String

-- SchemaDef 

data SchemaDef = SchemaDef { name :: String, schema :: Schema }

instance EncodeJson SchemaDef where
  encodeJson (SchemaDef schemaDef) = encodeJson
    { type: "json_schema"
    , json_schema:
        { name: schemaDef.name
        , schema: schemaDef.schema # encodeJson
        }
    }

data Schema
  = ObjectSchema (Object Schema)
  | ArraySchema Schema
  | StringSchema (Maybe String)
  | NumberSchema (Maybe String)

instance EncodeJson Schema where
  encodeJson (ObjectSchema properties) = encodeJson { type: "object", additionalProperties: false, required: properties # Object.keys, properties: properties # map encodeJson }
  encodeJson (ArraySchema items) = encodeJson { type: "array", items }
  encodeJson (StringSchema Nothing) = encodeJson { type: "string" }
  encodeJson (StringSchema (Just description)) = encodeJson { type: "string", description }
  encodeJson (NumberSchema Nothing) = encodeJson { type: "number" }
  encodeJson (NumberSchema (Just description)) = encodeJson { type: "number", description }

--------------------------------------------------------------------------------
-- endpoints
-- TODO: handle failure cases
--------------------------------------------------------------------------------

foreign import generate_ :: Json -> Effect (Promise Json)

generate :: { config :: Config, messages :: Array Msg } -> Aff AssistantMsg
generate args =
  ( toAffE $ generate_ $ encodeJson
      { baseURL: args.config.baseURL
      , model: args.config.model
      , apiKey: args.config.apiKey
      , messages: args.messages
      }
  ) >>=
    ( \response -> case response # decodeJson @{ content :: String } of
        Right { content } -> pure $ TextAssistantMsg { content }
        Left err -> throwError $ Aff.error $ "generate: " <> printJsonDecodeError err
    )

generate_tool :: { config :: Config, tools :: Array Tool, messages :: Array Msg } -> Aff (TextAssistantMsg \/ ToolAssistantMsg)
generate_tool args =
  ( toAffE $ generate_ $ encodeJson
      { baseURL: args.config.baseURL
      , model: args.config.model
      , apiKey: args.config.apiKey
      , tools: args.tools
      , tool_choice: "auto"
      , messages: args.messages
      }
  ) >>=
    ( \response -> case response # decodeJson @{ content :: String, tool_calls :: Array ToolCall } of
        Right { content, tool_calls } -> pure $ Right { content: Just content, toolCalls: tool_calls }
        Left err1 -> case response # decodeJson @{ tool_calls :: Array ToolCall } of
          Right { tool_calls } -> pure $ Right { content: Nothing, toolCalls: tool_calls }
          Left err2 -> case response # decodeJson @{ content :: String } of
            Right { content } -> pure $ Left { content }
            Left err3 -> throwError $ Aff.error $ "generate_tool:\n" <> Array.intercalate "\n" ([ err1, err2, err3 ] # map printJsonDecodeError)
    )

generate_structure :: { config :: Config, schemaDef :: SchemaDef, messages :: Array Msg } -> Aff StructureAssistantMsg
generate_structure args =
  ( toAffE $ generate_ $ encodeJson
      { baseURL: args.config.baseURL
      , model: args.config.model
      , apiKey: args.config.apiKey
      , messages: args.messages
      , response_format: args.schemaDef # encodeJson
      }
  ) >>=
    ( \response -> case response # decodeJson @{ content :: String } of
        Right { content } -> pure { parsed: parseJson content # either (\err -> bug $ "generate_structure: failed to parse content as JSON: " <> printJsonDecodeError err) identity }
        Left err -> throwError $ Aff.error $ "generate_structure: " <> printJsonDecodeError err
    )

