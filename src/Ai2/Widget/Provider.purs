-- A widget that lets you choose a provider for AI features.
module Ai2.Widget.Provider where

import Prelude

import Ai2.Llm as Llm
import Control.Monad.State (get, modify_)
import Control.Monad.Writer (tell)
import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Argonaut.Decode (fromJsonString, printJsonDecodeError)
import Data.Argonaut.Encode (toJsonString)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.Lens ((.=))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe')
import Data.String as String
import Data.Tuple.Nested ((/\))
import Data.Variant (match)
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import LocalStorage as LocalStorage
import Utility (bug, css, inj, prop, todo)

type Input =
  { providers :: Map String Provider
  }

type Provider =
  { baseURL :: String
  , models :: Array String
  }

providers_with_tools :: Map String Provider
providers_with_tools = Map.fromFoldable
  [ "openai" /\ { baseURL: "https://api.openai.com/v1", models: [ "gpt-4o" ] }
  , "ollama" /\ { baseURL: "http://localhost:11434/v1", models: [ "llama3-groq-tool-use:latest", "command-r7b:latest" ] }
  ]

component = H.mkComponent { initialState, eval, render }
  where

  initialState :: Input -> _
  initialState { providers } =
    { providers
    , status: "waiting for input"
    , open: true
    , provider: ""
    , model: ""
    , apiKey: ""
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure $ inj @"initialize" unit
    , handleAction = match
        { initialize: \_ -> do
            LocalStorage.load "LlmConfig" # liftEffect >>= case _ of
              Nothing -> pure unit
              Just string_config -> do
                case string_config # fromJsonString :: _ Llm.Config of
                  Left err -> modify_ _
                    { status = "Error when decoding saved config: " <> printJsonDecodeError err <> "."
                    }
                  Right config -> do
                    modify_ _
                      { status = "Successfully loaded saved config."
                      , open = false
                      }
                    H.raise config
                    pure unit
        , set_open: (prop @"open" .= _)
        , set_provider: (prop @"provider" .= _)
        , set_model:
            ( \model -> do
                Console.log $ "set_model " <> show model
                pure model
            ) >=> (prop @"model" .= _)
        , set_apiKey: (prop @"apiKey" .= _) >=> \_ -> submit
        , submit: \_ -> submit
        }
    }

  submit = do
    state <- get
    case state.providers # Map.lookup state.provider of
      Nothing -> do
        modify_ _ { status = "unrecognized provider: " <> show state.provider }
      Just provider -> do
        if String.null state.apiKey then
          modify_ _ { status = "you must provide an apiKey" }
        else if String.null state.model then
          modify_ _ { status = "you must select a model" }
        else if String.null state.provider then
          modify_ _ { status = "you must select a provider" }
        else do
          let config = { baseURL: provider.baseURL, model: state.model, apiKey: state.apiKey } :: Llm.Config
          Console.log $ stringifyWithIndent 4 $ encodeJson { config }
          LocalStorage.save "LlmConfig" (toJsonString config) # liftEffect
          modify_ _
            { status = "successfully saved new config"
            , open = false
            }
          H.raise config

  render { providers, status, open, provider, model } =
    if not open then
      HH.div
        [ HE.onClick \_ -> inj @"set_open" true
        , css do
            tell [ "background-color: black", "color: white", "user-select: none", "cursor: pointer" ]
            tell [ "padding: 0.5em" ]
        ]
        [ HH.text "Configure AI Arovider" ]
    else
      HH.div
        [ css do
            tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ]
            tell [ "padding: 0.5em" ]
        ]
        [ HH.div [] [ bold "Configure AI Provider" ]
        , HH.div [] [ HH.text "Your configuration will be saved to localStorage, and will be loaded automatically when you return to this page." ]

        , HH.div [] [ bold "Status: ", HH.text status ]

        , HH.div []
            [ bold "Provider: "
            , HH.select
                [ HP.value provider
                , HE.onValueChange $ inj @"set_provider"
                ] $ fold
                [ [ HH.option
                      [ HP.disabled true
                      , HP.selected true
                      ]
                      [ HH.text "select a provider" ]
                  ]
                , providers # Map.toUnfoldable # map \(provider' /\ _) -> HH.option [ HP.value provider' ] [ HH.text provider' ]
                ]
            ]

        , HH.div []
            [ bold "Model: "
            , case providers # Map.lookup provider of
                Nothing -> HH.div [] [ HH.text "waiting for provider choice" ]
                Just { models } ->
                  HH.select
                    [ HP.value model
                    , HE.onValueChange $ inj @"set_model"
                    ] $ fold $
                    [ [ HH.option
                          [ HP.disabled true
                          , HP.selected true
                          ]
                          [ HH.text "select a model" ]
                      ]
                    , models # map \model' -> HH.option [ HP.value model' ] [ HH.text model' ]
                    ]
            ]

        , HH.div []
            [ bold "API Key: "
            , HH.input [ HE.onValueChange $ inj @"set_apiKey" ]
            ]

        , HH.div
            [ HE.onClick \_ -> inj @"submit" unit
            , css do
                tell [ "background-color: black", "color: white", "user-select: none", "cursor: pointer" ]
                tell [ "padding: 0.5em" ]
            ]
            [ HH.text "Save" ]
        ]

bold s = HH.span [ css do tell [ "font-weight: bold" ] ] [ HH.text s ]

