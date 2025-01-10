module Test.Main where

import Prelude

import Ai.Llm (generate, noneToolChoice)
import Data.Either (Either(..))
import Data.Optional (defined, undefined_)
import Data.PartialRecord (PartialRecord(..))
import Data.TaggedUnion as TaggedUnion
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console

main :: Effect Unit
main = launchAff_ do
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

