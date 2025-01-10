module Test.Main where

import Prelude

import Ai.Llm (generate, noneToolChoice)
import Data.Argonaut.Encode (toJsonString)
import Data.Either (Either(..))
import Data.TaggedUnion as TaggedUnion
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console

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
    Left err -> Console.log $ "error: " <> err
    Right msg -> ?a
  Console.log $ "result: " <> toJsonString result
  pure unit

