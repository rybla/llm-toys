module Ai.Llm.Agent where

import Prelude

import Ai.Llm (Message, Tool, GenerateConfig, generate_basic, generate_with_tools)
import Data.Argonaut (Json, JsonDecodeError, parseJson, printJsonDecodeError, stringify)
import Data.Array (intercalate)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\))
import Data.Unfoldable (none)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Utility (inj)

newtype BasicAgent = BasicAgent
  { config :: GenerateConfig
  , messages :: Array Message
  }

generate_basic_agent :: BasicAgent -> Aff (BasicAgent /\ String)
generate_basic_agent (BasicAgent agent) = do
  reply <- generate_basic { config: agent.config, messages: agent.messages }
  pure $
    Tuple
      ( BasicAgent agent
          { messages = agent.messages `Array.snoc`
              (wrap $ inj @"assistant" { content: pure reply, tool_calls: none, parsed: none })
          }
      )
      reply

type StructureConfig a =
  { name :: String
  , forms ::
      Array
        { tool :: Tool
        , decode :: Json -> JsonDecodeError \/ a
        }
  }

generate_structure
  :: forall a
   . { config :: GenerateConfig
     , messages :: Array Message
     , structure_config :: StructureConfig a
     , max_tries :: Int
     }
  -> Aff a
generate_structure { config, messages, structure_config, max_tries } = loop max_tries []
  where
  loop i errs | i <= 0 = throwError $ Aff.error $
    "[generate_structure] failed too many times where max_tries=" <> show max_tries <> "; previous errors:" <>
      foldMap ("\n  - " <> _) errs
  loop i errs = do
    let try_again err = loop (i - 1) (errs `Array.snoc` err)
    case structure_config.forms # Array.head of
      Nothing -> throwError $ Aff.error $ "no forms in structure_config"
      {-
      -- if there's exactly 1 tool, then generate with format instead of tools
      Just form | Array.length structure_config.forms == 1 -> do
        result <- generate_with_format
          { config
          , messages
          , format: form.tool # unwrap # unwrap # _.parameters
          }
        case parseJson result of
          Left err -> try_again $ intercalate "\n"
            [ "tool_call argument string is not valid JSON:"
            , "    - error: " <> unindent (printJsonDecodeError err)
            , "    - args (string): " <> result
            ]
          Right args ->
            case form.decode args of
              Left err -> try_again $ intercalate "\n"
                [ "tool_call arguments are not valid:"
                , "    - error: " <> unindent (printJsonDecodeError err)
                , "    - args (json): " <> stringify args
                ]
              Right a -> pure a
      -}
      _ -> do
        result <- generate_with_tools
          { config
          , messages
          , tools: structure_config.forms # map _.tool
          , tool_choice: wrap $ inj @"required" unit
          }
        case result.tool_calls of
          Nothing -> try_again $ "undefined tool calls"
          Just tool_calls -> case Array.head tool_calls of
            Nothing -> try_again $ "no tool calls"
            Just tool_call -> do
              case structure_config.forms # Array.find (\form -> (form.tool # unwrap # unwrap).name == (unwrap tool_call).function.name) of
                Nothing -> try_again $ "no tool with name " <> show (unwrap tool_call).function.name
                Just form -> do
                  case parseJson (unwrap tool_call).function.arguments of
                    Left err -> try_again $ intercalate "\n"
                      [ "tool_call argument string is not valid JSON:"
                      , "    - error: " <> unindent (printJsonDecodeError err)
                      , "    - args (string): " <> (unwrap tool_call).function.arguments
                      ]
                    Right args ->
                      case form.decode args of
                        Left err -> try_again $ intercalate "\n"
                          [ "tool_call arguments are not valid:"
                          , "    - error: " <> unindent (printJsonDecodeError err)
                          , "    - args (json): " <> stringify args
                          ]
                        Right a -> pure a

unindent :: String -> String
unindent = String.replace (String.Pattern "\n") (String.Replacement " ")