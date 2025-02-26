module Main where

import Prelude

import Ai2.Llm as Llm
import Data.Argonaut (stringifyWithIndent)
import Data.Argonaut.JsonSchema (class DecodeJsonFromSchema, class ToJsonSchema, generic_decodeJsonFromSchema, generic_toJsonSchema, toJsonSchema)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console

data WarningLevel = Low | Medium | High

derive instance Generic WarningLevel _

instance Show WarningLevel where
  show x = genericShow x

instance ToJsonSchema WarningLevel where
  toJsonSchema = generic_toJsonSchema @WarningLevel

instance DecodeJsonFromSchema WarningLevel where
  decodeJsonFromSchema x = generic_decodeJsonFromSchema x

main :: Effect Unit
main = do
  launchAff_ do
    let
      test :: forall a. Show a => _ (_ a) -> _ Unit
      test ma = ma >>= case _ of
        Left err -> Console.error err
        Right a -> Console.logShow a

    -- test $ Llm.generate_structure @(name :: String, age :: Int)
    --   { config: { apiKey: "ollama", baseURL: "http://localhost:11434/v1", model: "llama3-groq-tool-use:latest" }
    --   , name: "person"
    --   , messages: [ Llm.mkUserMsg "Generate a person with a cool-sounding name and a reasonable age." ]
    --   }

    -- test $ Llm.generate_structure @(people :: Array { name :: String, age :: Int })
    --   { config: { apiKey: "ollama", baseURL: "http://localhost:11434/v1", model: "llama3-groq-tool-use:latest" }
    --   , name: "people"
    --   , messages: [ Llm.mkUserMsg "Generate 10 people with assorted names and ages." ]
    --   }

    Console.log $ stringifyWithIndent 4 $ toJsonSchema @WarningLevel

    test $ Llm.generate_structure @(warning :: WarningLevel)
      { config: { apiKey: "ollama", baseURL: "http://localhost:11434/v1", model: "llama3-groq-tool-use:latest" }
      , name: "WarningLevel"
      , messages: [ Llm.mkUserMsg "Choose a random WarningLevel." ]
      }
    pure unit

