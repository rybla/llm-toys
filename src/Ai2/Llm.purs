module Ai2.Llm
  ( Config
  , Msg(..)
  , SystemMsg
  , mkSystemMsg
  , UserMsg
  , mkUserMsg
  , AssistantMsg
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
  , SchemaDef(..)
  , mkSchemaDef
  , Schema(..)
  , StringRestriction
  , mkObjectSchema
  , mkArraySchema
  , mkStringSchema
  , mkNumberSchema
  , mkBooleanSchema
  -- generate functions
  , generate
  , generate_tool
  , generate_structure
  , generate_structure'
  ) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, parseJson, stringify, stringifyWithIndent)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.JsonSchema (class DecodeJsonFromSchema, class ToJsonSchema, decodeJsonFromSchema, toJsonSchema)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Debug as Debug
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Foreign.Object (Object)
import Foreign.Object as Object
import Record as R
import Type.Row.Homogeneous (class Homogeneous)

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

data SchemaDef = SchemaDef { name :: String, schema :: Object Schema }

mkSchemaDef ∷ forall r. Homogeneous r Schema ⇒ String → Record r → SchemaDef
mkSchemaDef name schema = SchemaDef { name, schema: Object.fromHomogeneous schema }

instance EncodeJson SchemaDef where
  encodeJson (SchemaDef schemaDef) = encodeJson
    { type: "json_schema"
    , json_schema:
        { name: schemaDef.name
        , schema: ObjectSchema schemaDef.schema
        }
    }

-- TODO: make this intrinsically typed
data Schema
  = ObjectSchema (Object Schema)
  | ArraySchema Schema
  | StringSchema { description :: Maybe String, restriction :: Maybe StringRestriction }
  | NumberSchema { description :: Maybe String }
  | BooleanSchema { description :: Maybe String }
  | UnionSchema (Map String Schema)

data StringRestriction
  = EnumStringRestriction (Array String)
  | ConstStringRestriction String

mkObjectSchema :: forall r. Homogeneous r Schema => Record r -> Schema
mkObjectSchema r = ObjectSchema $ Object.fromHomogeneous r

mkArraySchema = ArraySchema

mkStringSchema args_ = StringSchema args
  where
  args = args_ `R.merge` { description: Nothing @String, restriction: Nothing @StringRestriction }

mkNumberSchema args_ = NumberSchema args
  where
  args = args_ `R.merge` { description: Nothing @String }

mkBooleanSchema args_ = BooleanSchema args
  where
  args = args_ `R.merge` { description: Nothing @String }

instance EncodeJson Schema where
  encodeJson (ObjectSchema properties) = encodeJson { type: "object", additionalProperties: false, required: properties # Object.keys, properties: properties # map encodeJson }

  encodeJson (ArraySchema items) = encodeJson { type: "array", items }

  encodeJson (StringSchema { description: Nothing, restriction: Nothing }) = encodeJson { type: "string" }
  encodeJson (StringSchema { description: Nothing, restriction: Just (EnumStringRestriction enum) }) = encodeJson { type: "string", enum }
  encodeJson (StringSchema { description: Nothing, restriction: Just (ConstStringRestriction const) }) = encodeJson { type: "string", const }
  encodeJson (StringSchema { description: Just description, restriction: Nothing }) = encodeJson { type: "string", description }
  encodeJson (StringSchema { description: Just description, restriction: Just (EnumStringRestriction enum) }) = encodeJson { type: "string", enum, description }
  encodeJson (StringSchema { description: Just description, restriction: Just (ConstStringRestriction const) }) = encodeJson { type: "string", const, description }

  encodeJson (NumberSchema { description: Nothing }) = encodeJson { type: "number" }
  encodeJson (NumberSchema { description: Just description }) = encodeJson { type: "number", description }

  encodeJson (BooleanSchema { description: Nothing }) = encodeJson { type: "boolean" }
  encodeJson (BooleanSchema { description: Just description }) = encodeJson { type: "boolean", description }

  encodeJson (UnionSchema forms) = encodeJson
    { anyOf: forms # (Map.toUnfoldable :: _ -> Array _) # map \(tag /\ value) ->
        ObjectSchema $ Object.fromHomogeneous { tag: StringSchema { description: none, restriction: Just (ConstStringRestriction tag) }, value }
    }

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

generate_tool :: { config :: Config, tools :: Array Tool, messages :: Array Msg } -> Aff (String \/ (TextAssistantMsg \/ ToolAssistantMsg))
generate_tool args =
  ( toAffE $ generate_ { error: Left, ok: Right } $ encodeJson
      { baseURL: args.config.baseURL
      , model: args.config.model
      , apiKey: args.config.apiKey
      , tools: args.tools
      , tool_choice: "auto"
      , messages: args.messages
      }
  ) <#> case _ of
    Left err -> Left $ "generate_tool: " <> err
    Right response -> case response # decodeJson @{ content :: String, tool_calls :: Array ToolCall } of
      Right { content, tool_calls } -> Right $ Right { content: Just content, toolCalls: tool_calls }
      Left err1 -> case response # decodeJson @{ tool_calls :: Array ToolCall } of
        Right { tool_calls } -> Right $ Right { content: Nothing, toolCalls: tool_calls }
        Left err2 -> case response # decodeJson @{ content :: String } of
          Right { content } -> Right $ Left { content }
          Left err3 -> Left $ "generate_tool:\n" <> Array.intercalate "\n" ([ err1, err2, err3 ] # map printJsonDecodeError)

generate_structure :: { config :: Config, schemaDef :: SchemaDef, messages :: Array Msg } -> Aff (String \/ StructureAssistantMsg)
generate_structure args =
  ( toAffE $ generate_ { error: Left, ok: Right } $ encodeJson
      { baseURL: args.config.baseURL
      , model: args.config.model
      , apiKey: args.config.apiKey
      , messages: args.messages
      , response_format: args.schemaDef # encodeJson
      }
  ) <#> case _ of
    Left err -> Left $ "generate_structure: " <> err
    Right response -> case response # decodeJson @{ content :: String } of
      Right { content } -> case parseJson content of
        Left err -> Left $ printJsonDecodeError err
        Right parsed -> Right { parsed }
      Left err -> Left $ "generate_structure: failed to parsed content as JSON: " <> printJsonDecodeError err

generate_structure'
  :: forall @r
   . ToJsonSchema (Record r)
  => DecodeJsonFromSchema (Record r)
  => { config :: Config, name :: String, messages :: Array Msg }
  -> Aff (String \/ Record r)
generate_structure' args = do
  Debug.traceM $ "schema: " <> stringifyWithIndent 4 (toJsonSchema @(Record r))
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
      Right { content } -> do
        Debug.traceM $ "generated content: " <> content
        case parseJson content of
          Left err -> Left $ printJsonDecodeError err
          Right parsed -> do
            Debug.traceM $ "successfully generated parsed json: " <> stringifyWithIndent 4 parsed
            case parsed # decodeJsonFromSchema of
              Left err -> Left $ printJsonDecodeError err
              Right a -> pure a
      Left err -> Left $ "generate_structure: failed to parsed content as JSON: " <> printJsonDecodeError err

