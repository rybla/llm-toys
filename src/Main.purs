module Main where

import Prelude

import Ai2.Llm (SchemaDef(..))
import Ai2.Llm as Llm
import Data.Argonaut (class DecodeJson, decodeJson, encodeJson, stringifyWithIndent)
import Data.Argonaut.JsonSchema (class DecodeJsonFromSchema, class ToJsonSchema, decodeJsonFromSchema, generic_decodeJsonFromSchema, generic_toJsonSchema, toJsonSchema)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console

data MyType = MyString String | MyInt Int | MyBoolean Boolean

derive instance Generic MyType _

instance Show MyType where
  show x = genericShow x

instance ToJsonSchema MyType where
  toJsonSchema = generic_toJsonSchema @MyType

instance DecodeJsonFromSchema MyType where
  decodeJsonFromSchema x = generic_decodeJsonFromSchema x

main :: Effect Unit
main = do
  -- let v = MyString "hello world"
  -- let s = toTaggedSum $ Generic.from v
  -- Console.log $ show s
  -- Console.log $ toJsonString s

  -- let v = MyString "hello world"
  -- Console.log $ toJsonString $ AsTaggedSum v

  Console.log $ stringifyWithIndent 4 $ toJsonSchema @MyType
  Console.log $ show $ decodeJsonFromSchema @MyType $ encodeJson { type: "MyString", value: "hello world" }
  Console.log $ show $ decodeJsonFromSchema @MyType $ encodeJson { type: "MyInt", value: 1 }
  Console.log $ show $ decodeJsonFromSchema @MyType $ encodeJson { type: "MyBoolean", value: true }

  launchAff_ do
    result <- Llm.generate_structure' @(name :: String, age :: Int)
      { config: { apiKey: "ollama", baseURL: "http://localhost:11434/v1", model: "llama3-groq-tool-use:latest" }
      , name: "person"
      , messages:
          [ Llm.mkUserMsg "Generate a person with a cool-sounding name and a reasonable age."
          ]
      }
    Console.logShow result
    pure unit
