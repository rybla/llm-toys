module Ai.Llm.Agent where

import Prelude

import Ai.Llm (GenerateConfig, Message, Tool, generate)
import Data.Argonaut (Json, parseJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Utility (inj)

type GenerateStructure a =
  { name :: String
  , tools ::
      Array
        { tool :: Tool
        , decode :: Json -> String \/ a
        }
  }

generate_structure
  :: forall a
   . { config :: GenerateConfig
     , messages :: Array Message
     , structure :: GenerateStructure a
     , max_tries :: Int
     }
  -> Aff a
generate_structure { config, messages, structure, max_tries } = loop max_tries []
  where
  loop i errs | i <= 0 = throwError $ Aff.error $
    "[generate_structure] failed too many times where max_tries=" <> show max_tries <> "; previous errors:" <>
      foldMap ("\n  - " <> _) errs
  loop i errs = do
    let try_again err = loop (i - 1) (errs `Array.snoc` err)
    result <- generate
      { config
      , messages
      , tools: structure.tools # map _.tool
      , tool_choice: wrap $ inj @"required" unit
      }
    case result.tool_calls # Array.head of
      Nothing -> try_again $ "no tool calls"
      Just tool_call -> do
        -- find the tool in structure.tools with the appropriate name
        case structure.tools # Array.find (\tool -> (unwrap tool.tool).name == (unwrap tool_call).function.name) of
          Nothing -> try_again $ "no tool with name " <> show (unwrap tool_call).function.name
          Just tool -> do
            case parseJson (unwrap tool_call).function.arguments of
              Left err -> try_again $ "tool_call argument string is not valid JSON: " <> printJsonDecodeError err
              Right args ->
                case tool.decode args of
                  Left err -> try_again $ "tool_call arguments are not valid: " <> err
                  Right a -> pure a
