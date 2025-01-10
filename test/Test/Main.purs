module Test.Main where

import Prelude

import Ai.Llm (generate, noneToolChoice)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.Optional (defined)
import Data.PartialRecord (PartialRecord(..))
import Data.TaggedUnion as TaggedUnion
import Data.Variant (case_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Utility (on)

main :: Effect Unit
main = launchAff_ do
  result <- generate $ PartialRecord
    { apiKey: "ollama"
    , baseURL: "http://localhost:11434/v1" # defined
    , model: "phi4"
    , messages: [ TaggedUnion.make @_ @"user" $ PartialRecord { content: "What is 2 + 3?" } ]
    , tools: [] # defined
    , tool_choice: noneToolChoice # defined
    }
  case result of
    Left err -> do
      Console.log $ "error: " <> err
    Right msg ->
      unwrap msg #
        ( case_ # on @"assistant" \(PartialRecord { content, tool_calls: _ }) -> do
            Console.log $ "result: " <> show content
        )
  pure unit

