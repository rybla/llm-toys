-- A widget that lets you choose a provider for AI features.
module Ai2.Widget.Provider where

import Prelude

import Ai2.Llm as Llm
import Control.Monad.State (get, modify_)
import Control.Monad.Writer (tell)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Argonaut (JsonDecodeError, encodeJson, stringifyWithIndent)
import Data.Argonaut.Decode (fromJsonString, printJsonDecodeError)
import Data.Argonaut.Encode (toJsonString)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Foldable (fold)
import Data.Lens ((.=))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple.Nested ((/\))
import Data.Variant (match)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import LocalStorage as LocalStorage
import Utility (css, inj, prop)

type Input =
  { providerCategory :: String
  , providers :: Map String Provider
  }

type Provider =
  { baseURL :: String
  , model :: String
  }

mkOpenaiProvider model = { baseURL: "https://api.openai.com/v1", model } :: Provider
mkGoogleProvider model = { baseURL: "https://generativelanguage.googleapis.com/v1beta/openai", model } :: Provider
mkOllamaProvider model = { baseURL: "http://localhost:11434/v1", model } :: Provider

type ProviderConfig =
  { name :: String
  , config :: Llm.Config
  }

providers :: Map String Provider
providers = Map.fromFoldable
  [ "openai / gpt-4o" /\ mkOpenaiProvider "gpt-4o"
  , "openai / gpt-4o-mini" /\ mkOpenaiProvider "gpt-4o-mini"
  , "openai / o1" /\ mkOpenaiProvider "o1"
  , "openai / o1-mini" /\ mkOpenaiProvider "o1-mini"
  , "openai / o3-mini" /\ mkOpenaiProvider "o3-mini"
  , "openai / o3-preview" /\ mkOpenaiProvider "o3-preview"
  , "openai / gpt-4-turbo" /\ mkOpenaiProvider "gpt-4-turbo"
  , "openai / gpt-4" /\ mkOpenaiProvider "gpt-4"
  , "openai / gpt-3.5-turbo" /\ mkOpenaiProvider "gpt-3.5-turbo"
  , "openai / gpt-3.5-turbo-instruct" /\ mkOpenaiProvider "gpt-3.5-turbo-instruct"
  , "ollama / gemma2:9b" /\ mkOllamaProvider "gemma2:9b"
  , "ollama / deepseek-r1:14b" /\ mkOllamaProvider "deepseek-r1:14b"
  , "ollama / deepscalar:latest" /\ mkOllamaProvider "deepscalar:latest"
  , "ollama / llama3-groq-tool-use:latest" /\ mkOllamaProvider "llama3-groq-tool-use:latest"
  , "ollama / command-r7b:latest" /\ mkOllamaProvider "command-r7b:latest"
  , "google / gemini-2.0-flash" /\ mkGoogleProvider "gemini-2.0-flash"
  ]

providers_with_structured_output :: Map String Provider
providers_with_structured_output = Map.fromFoldable
  [ "openai / gpt-4o" /\ mkOpenaiProvider "gpt-4o"
  , "ollama / llama3-groq-tool-use:latest" /\ mkOllamaProvider "llama3-groq-tool-use:latest"
  , "ollama / command-r7b:latest" /\ mkOllamaProvider "command-r7b:latest"
  , "google / gemini-2.0-flash" /\ mkGoogleProvider "gemini-2.0-flash"
  ]

providers_with_tools :: Map String Provider
providers_with_tools = Map.fromFoldable
  [ "openai / gpt-4o" /\ mkOpenaiProvider "gpt-4o"
  , "ollama / llama3-groq-tool-use:latest" /\ mkOllamaProvider "llama3-groq-tool-use:latest"
  , "ollama / command-r7b:latest" /\ mkOllamaProvider "command-r7b:latest"
  , "google / gemini-2.0-flash" /\ mkGoogleProvider "gemini-2.0-flash"
  ]

component
  ∷ ∀ (query ∷ Type -> Type)
  . H.Component
      query
      { providerCategory ∷ String, providers ∷ Map String { baseURL :: String, model :: String } }
      { apiKey ∷ String, baseURL ∷ String, model ∷ String }
      Aff
component = H.mkComponent { initialState, eval, render }
  where

  initialState :: Input -> _
  initialState { providerCategory, providers } =
    { providerCategory
    , providerConfig_localStorageKey: "AiProvider_" <> providerCategory
    , savedProviderConfigs_localStorageKey: "SavedAiProviders_" <> providerCategory
    , providers
    , status: "waiting for input"
    , open: true
    , provider: ""
    , apiKey: ""
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure $ inj @"initialize" unit
    , handleAction = match
        { initialize: \_ -> do
            getProviderConfig >>= case _ of
              Nothing -> pure unit
              Just providerConfig -> do
                modify_ _
                  { status = "Successfully loaded saved provider: " <> providerConfig.name
                  , provider = providerConfig.name
                  , apiKey = providerConfig.config.apiKey
                  , open = false
                  }
                H.raise providerConfig.config
        , set_open: (prop @"open" .= _)
        , set_provider: setProvider
        , set_apiKey: (prop @"apiKey" .= _) >=> \_ -> submit
        , submit: \_ -> submit
        }
    }

  getSavedProviderConfigs :: _ (Map String Llm.Config)
  getSavedProviderConfigs = do
    state <- get
    LocalStorage.load state.savedProviderConfigs_localStorageKey # liftEffect >>= case _ of
      Nothing -> pure Map.empty
      Just savedProviderConfigs_string -> case savedProviderConfigs_string # fromJsonString of
        Left _ -> pure Map.empty
        Right savedProviderConfigs -> pure savedProviderConfigs

  insertSavedProviderConfig provider config = do
    savedProviderConfigs <- getSavedProviderConfigs
    state <- get
    LocalStorage.save state.savedProviderConfigs_localStorageKey (toJsonString (savedProviderConfigs # Map.insert provider config)) # liftEffect

  setProvider provider = do
    modify_ _ { provider = provider }
    savedProviderConfigs <- getSavedProviderConfigs
    case savedProviderConfigs # Map.lookup provider of
      Nothing -> modify_ _
        { apiKey = "" }
      Just config -> modify_ _
        { apiKey = config.apiKey
        , status = "Successfully loaded saved provider: " <> provider
        }

  getProviderConfig :: _ (Maybe ProviderConfig)
  getProviderConfig = do
    state <- get
    LocalStorage.load state.providerConfig_localStorageKey # liftEffect >>= case _ of
      Nothing -> pure Nothing
      Just string_providerConfig -> do
        case string_providerConfig # fromJsonString :: JsonDecodeError \/ ProviderConfig of
          Left err -> do
            modify_ _ { status = "Error when decoding saved provider: " <> printJsonDecodeError err <> "." }
            pure Nothing
          Right providerConfig -> do
            pure $ Just providerConfig

  setProviderConfig :: ProviderConfig -> _ Unit
  setProviderConfig providerConfig = do
    state <- get
    LocalStorage.save state.providerConfig_localStorageKey (toJsonString providerConfig) # liftEffect

  submit = do
    state <- get
    case state.providers # Map.lookup state.provider of
      Nothing -> do
        modify_ _ { status = "unrecognized provider: " <> show state.provider }
      Just provider -> do
        if String.null state.apiKey then
          modify_ _ { status = "you must provide an apiKey" }
        else if String.null state.provider then
          modify_ _ { status = "you must select a provider" }
        else do
          let config = { baseURL: provider.baseURL, model: provider.model, apiKey: state.apiKey } :: Llm.Config
          let providerConfig = { name: state.provider, config } :: ProviderConfig
          Console.log $ "new provider for " <> state.providerCategory <> ": " <> (stringifyWithIndent 4 $ encodeJson providerConfig)
          insertSavedProviderConfig state.provider config
          setProviderConfig providerConfig
          modify_ _
            { status = "successfully saved new provider config: " <> state.provider
            , open = false
            }
          H.raise config

  render { providerCategory, providers, status, open, provider, apiKey } =
    if not open then
      HH.div
        [ HE.onClick \_ -> inj @"set_open" true
        , css do
            tell [ "background-color: black", "color: white", "user-select: none", "cursor: pointer" ]
            tell [ "padding: 0.5em" ]
        ]
        [ HH.text $ "Configure AI Provider for " <> providerCategory ]
    else
      HH.div
        [ css do
            tell [ "display: flex", "flex-direction: column", "gap: 0.5em" ]
            tell [ "padding: 0.5em" ]
        ]
        [ HH.div [] [ bold $ "Configure AI Provider for " <> providerCategory ]
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
                      , HP.value ""
                      ]
                      [ HH.text "select a provider" ]
                  ]
                , providers # Map.toUnfoldable # map \(name /\ _) -> HH.option [ HP.value name ] [ HH.text name ]
                ]
            ]

        , HH.div []
            [ bold "API Key: "
            , HH.input
                [ HP.value apiKey
                , HE.onValueChange $ inj @"set_apiKey"
                , HP.type_ InputPassword
                ]
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

