module Data.Argonaut.JsonSchemaTest where

import Prelude

import Ai2.Llm as Llm
import Data.Argonaut.JsonSchema (class DecodeJsonFromSchema, class ToJsonSchema, generic_decodeJsonFromSchema, generic_toJsonSchema)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console

-- type Person =
--   { name :: String
--   , age :: Int
--   }

data Value = String String | Int Int

derive instance Generic Value _

instance Show Value where
  show x = genericShow x

instance ToJsonSchema Value where
  toJsonSchema = generic_toJsonSchema @Value

instance DecodeJsonFromSchema Value where
  decodeJsonFromSchema x = generic_decodeJsonFromSchema x

main :: Effect Unit
main = launchAff_ do
  person <- Llm.generate_structure @(value :: Value)
    { config:
        { apiKey: "AIzaSyDHzXdhzpqf_ZCXG5a-PMderx"
        , baseURL: "https://generativelanguage.googleapis.com/v1beta/openai"
        , model: "gemini-2.0-flash"
        }
    , name: "Value"
    , messages:
        [ Llm.mkUserMsg "Generate an example value."
        ]
    }
  Console.logShow { person }
