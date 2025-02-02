module Test.Main where

import Prelude

import Ai.Llm.Agent as Agent
import Ai.Llm.Core (generate, noneToolChoice)
import Data.Argonaut (decodeJson)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Newtype (wrap)
import Data.Optional (defined, undefined_)
import Data.PartialRecord (PartialRecord(..))
import Data.TaggedUnion as TaggedUnion
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import Utility (inj)

main :: Effect Unit
main = launchAff_ do
  -- basic_test
  generate_structure_test

basic_test :: Aff Unit
basic_test = do
  result <- generate $ PartialRecord
    { apiKey: "ollama"
    , baseURL: "http://localhost:11434/v1" # defined
    , model: "phi4"
    , messages:
        [ TaggedUnion.make @_ @"user" $ PartialRecord
            { name: undefined_
            , content:
                "What is 2 + 3? Reply with just the numeric result."
            }
        ]
    , tools: [] # defined
    , tool_choice: noneToolChoice # defined
    }
  case result of
    Left err -> do
      Console.log $ "error: " <> err
    Right (PartialRecord { content, tool_calls: _ }) ->
      Console.log $ "result: " <> show content
  pure unit

generate_structure_test :: Aff Unit
generate_structure_test = do
  result <- Agent.generate_structure
    { config:
        { apiKey: "ollama"
        , baseURL: "http://localhost:11434/v1"
        , model: "llama3-groq-tool-use"
        -- , model: "llama3.2:latest"
        }
    , messages:
        [ wrap $ inj @"user"
            { name: none
            , content:
                "Please select a positive integer. Make sure to use a tool for your response."
            }
        ]
    , structure_config:
        { name: "positive integer"
        , forms:
            [ { tool: wrap $ wrap $
                  { name: "select_positive_integer"
                  , description: "Selects a positive integer."
                  , parameters: wrap $ inj @"object" $ wrap $ Map.fromFoldable
                      [ "x" /\ wrap (inj @"number" { description: "A positive integer." }) ]
                  }
              , decode: decodeJson @{ "x" :: Number }
              }
            ]
        }
    , max_tries: 5
    }
  Console.logShow { result }
  pure unit