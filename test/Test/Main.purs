module Test.Main where

import Prelude

import Ai.Llm (generate, noneToolChoice)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Data.TaggedUnion as TaggedUnion
import Data.Variant (case_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Utility (on)

main :: Effect Unit
main = launchAff_ do
  result <- generate
    { apiKey: "ollama"
    , baseURL: "http://localhost:11434/v1"
    , model: "phi4"
    , messages: [ TaggedUnion.make @_ @"user" { content: "What is 2 + 3?" } ]
    , tools: []
    , tool_choice: noneToolChoice
    }
  case result of
    Left err -> do
      Console.log $ "error: " <> err
    Right msg ->
      unwrap msg #
        ( case_ # on @"assistant" \{ content, tool_calls: _ } -> do
            Console.log $ "result: " <> content
        )
  pure unit

