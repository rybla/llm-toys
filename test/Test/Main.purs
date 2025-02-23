module Test.Main where

import Prelude

import Ai2.Llm (Schema(..))
import Ai2.Llm as Llm
import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Argonaut.Encode (toJsonString)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (none)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console as Console
import Foreign.Object as Object
import Secret (apiKey_openai, baseURL_openai)

main :: Effect Unit
main = launchAff_ do
  let config = { baseURL: baseURL_openai, apiKey: apiKey_openai, model: "gpt-4o" } :: Llm.Config

  when false do
    Llm.generate
      { config
      , messages: [ Llm.UserMsg { content: "Write a single sentence description of a banana." } ]
      }
      >>= \reply -> Console.log $ "Llm.generate ==> " <> (reply # toJsonString # show)

  when false do
    Llm.generate_tool
      { config
      , tools:
          [ Llm.FunctionTool
              { name: "set_count"
              , description: "Sets the current count."
              , parameters: ObjectSchema (Object.fromFoldable [ "count" /\ NumberSchema none ])
              }
          ]
      , messages: [ Llm.UserMsg { content: "Set the count to 100." } ]
      }
      >>= \reply -> Console.log $ "Llm.generate_tool ==> " <> (reply # encodeJson # stringifyWithIndent 4)

  when false do
    Llm.generate_structure
      { config
      , schemaDef: Llm.SchemaDef { name: "birthday_party_locations", schema: ObjectSchema (Object.fromFoldable [ "birthday_party_locations" /\ ArraySchema (StringSchema ("A single birthday party location" # pure)) ]) }
      , messages: [ Llm.UserMsg { content: "Make a list of reasonable locations to hold a birthday party. Just say the general kinds of places, no need to be super specific." } ]
      }
      >>= \reply -> Console.log $ "Llm.generate_structure ==> " <> (reply # encodeJson # stringifyWithIndent 4)

  pure unit

-- import Prelude

-- import Ai.Llm (Structure(..))
-- import Ai.Llm.Agent as Agent
-- import Ai.Llm.Core (generate, noneToolChoice)
-- import Data.Argonaut (decodeJson)
-- import Data.Either (Either(..))
-- import Data.Map as Map
-- import Data.Newtype (wrap)
-- import Data.Optional (defined, undefined_)
-- import Data.PartialRecord (PartialRecord(..))
-- import Data.TaggedUnion as TaggedUnion
-- import Data.Tuple.Nested ((/\))
-- import Data.Unfoldable (none)
-- import Effect (Effect)
-- import Effect.Aff (Aff, launchAff_)
-- import Effect.Class.Console as Console
-- import Utility (inj)
-- main :: Effect Unit
-- main = launchAff_ do
--   -- basic_test
--   generate_structure_test

-- basic_test :: Aff Unit
-- basic_test = do
--   result <- generate $ PartialRecord
--     { apiKey: "ollama"
--     , baseURL: "http://localhost:11434/v1" # defined
--     , model: "phi4"
--     , messages:
--         [ TaggedUnion.make @_ @"user" $ PartialRecord
--             { name: undefined_
--             , content:
--                 "What is 2 + 3? Reply with just the numeric result."
--             }
--         ]
--     , tools: [] # defined
--     , tool_choice: noneToolChoice # defined
--     }
--   case result of
--     Left err -> do
--       Console.log $ "error: " <> err
--     Right (PartialRecord { content, tool_calls: _ }) ->
--       Console.log $ "result: " <> show content
--   pure unit

-- generate_structure_test :: Aff Unit
-- generate_structure_test = do
--   result <- Agent.generate_structure
--     { config:
--         { apiKey: "ollama"
--         , baseURL: "http://localhost:11434/v1"
--         , model: "llama3-groq-tool-use"
--         -- , model: "llama3.2:latest"
--         }
--     , messages:
--         [ wrap $ inj @"user"
--             { name: none
--             , content:
--                 "Please select a positive integer. Make sure to use a tool for your response."
--             }
--         ]
--     , structure_config:
--         { name: "positive integer"
--         , forms:
--             [ { tool: wrap $ wrap $
--                   { name: "select_positive_integer"
--                   , description: "Selects a positive integer."
--                   , parameters: Structure $ inj @"object" $ wrap $ Map.fromFoldable
--                       [ "x" /\ Structure (inj @"number" { description: "A positive integer." }) ]
--                   }
--               , decode: decodeJson @{ "x" :: Number }
--               }
--             ]
--         }
--     , max_tries: 5
--     }
--   Console.logShow { result }
--   pure unit