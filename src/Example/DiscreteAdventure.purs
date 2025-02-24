module Example.DiscreteAdventure where

import Prelude

import Ai2.Llm (Config)
import Ai2.Llm as Llm
import Ai2.Widget.Provider as Provider
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (get, modify_)
import Control.Monad.Writer (tell)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array as Array
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Lens ((.=))
import Data.Maybe (Maybe(..), maybe')
import Data.Traversable (traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Unfoldable (none)
import Data.Variant (Variant, match)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Utility (css, downloadMarkdownTextFile, inj, prop, scrollIntoView)
import Web.HTML.HTMLElement as Web.HTML.HTMLElement

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type Input world =
  { engine :: Engine world
  }

type State world =
  { config :: Maybe Llm.Config
  , engine :: Engine world
  , world :: world
  , transcript :: Array (StoryEvent world)
  , generating_StoryEvent_status ::
      Variant
        ( generating :: StoryChoice world
        , error :: String
        , done :: Unit
        )
  , choices ::
      Variant
        ( generating :: Unit
        , waiting_for_story :: Unit
        , error :: String
        , done :: Array (StoryChoice world)
        )
  }

type Engine world =
  { initial_world :: world
  , renderWorld :: world -> PlainHTML
  , initial_transcript :: Array (StoryEvent world)
  , initial_choices :: Array (StoryChoice world)
  , prompt_StoryEvent :: world -> StoryChoice world -> Aff { system :: String, user :: String }
  , prompt_StoryChoices :: world -> Array (StoryEvent world) -> Aff { system :: String, user :: String }
  , printStory :: world -> Array (StoryEvent world) -> String
  }

type StoryEvent world =
  { choice :: StoryChoice world
  , description :: String
  }

type StoryChoice world =
  { short_description :: String
  , description :: String
  , update :: WorldUpdate world
  }

type WorldUpdate world =
  { apply :: world -> world
  , description :: String
  }

type Action world = Variant
  ( submit_choice :: StoryChoice world
  , set_config :: Maybe Config
  , save_story :: Unit
  , load_story :: Unit
  )

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

main_width = 1200
main_height = 600

main_component ∷ forall world query output. H.Component query (Input world) output Aff
main_component = H.mkComponent { initialState, eval, render }
  where
  initialState :: _ -> State world
  initialState { engine } =
    { config: none
    , engine
    , world: engine.initial_world
    , transcript: engine.initial_transcript
    , generating_StoryEvent_status: inj @"done" unit
    , choices: inj @"done" engine.initial_choices
    }

  eval = H.mkEval { handleAction, handleQuery: \_query -> pure none, receive: \_input -> none, initialize: none, finalize: none }

  getConfig = get >>= \{ config } -> case config of
    Nothing -> throwError $ Aff.error "you must configure your AI provider first"
    Just config' -> pure config'

  scrollDownStory = do
    H.getHTMLElementRef ref_lastStoryItem >>= case _ of
      Nothing -> pure unit
      Just e -> do
        e # Web.HTML.HTMLElement.toElement # scrollIntoView # liftEffect
        pure unit

  handleAction = match
    { set_config: (prop @"config" .= _)
    , submit_choice: \choice -> do
        Console.log $ "submit_choice: " <> show choice.short_description
        -- generate StoryEvent
        do
          modify_ _
            { generating_StoryEvent_status = inj @"generating" choice
            , choices = inj @"waiting_for_story" unit
            }
          scrollDownStory
          { engine, world, transcript } <- get
          config <- getConfig
          prompt_StoryEvent <- engine.prompt_StoryEvent world choice # liftAff
          description <- do
            response <-
              Llm.generate
                { config: config
                , messages:
                    [ [ Llm.mkSystemMsg prompt_StoryEvent.system ]
                    , transcript # foldMap \event ->
                        [ Llm.mkUserMsg event.choice.description
                        , Llm.mkTextAssistantMsg event.description
                        ]
                    , [ Llm.mkUserMsg prompt_StoryEvent.user ]
                    ] # fold
                } # liftAff
            case response of
              Left err -> do
                modify_ _
                  { generating_StoryEvent_status = inj @"error" err
                  , choices = inj @"error" "error when generating next StoryEvent"
                  }
                scrollDownStory
                throwError $ Aff.error $ err
              Right { content } -> pure content
          modify_ \env -> env
            { transcript = env.transcript `Array.snoc` { choice, description }
            , generating_StoryEvent_status = inj @"done" unit
            , choices = inj @"generating" unit
            }
          scrollDownStory
          pure unit
        -- generate StoryChoices
        do
          { engine, world, transcript } <- get
          config <- getConfig
          prompt_StoryChoices <- engine.prompt_StoryChoices world transcript # liftAff
          err_choices :: Either JsonDecodeError Prompt_StoryChoices_Structure <- do
            response <-
              Llm.generate_structure
                { config: config
                , schemaDef: prompt_StoryChoices_schemaDef
                , messages:
                    [ Llm.mkSystemMsg prompt_StoryChoices.system
                    , Llm.mkUserMsg prompt_StoryChoices.user
                    ]
                } # liftAff
            case response of
              Left err -> do
                modify_ _ { choices = inj @"error" err }
                throwError $ Aff.error $ err
              Right { parsed } -> pure $ parsed # decodeJson
          case err_choices of
            Left err -> Console.error $ "Failed to generate choices: " <> printJsonDecodeError err
            Right { choices } ->
              modify_ _
                { choices = inj @"done" $ choices # map \{ long_description, short_description } ->
                    { description: long_description
                    , short_description
                    , update: { apply: identity, description: "TODO: update.description (at some point, the function that updates the world will be inferred from a text rule description given by the LLM)" }
                    }
                }
          pure unit
    , save_story: \_ -> do
        state <- get
        let text = state.engine.printStory state.world state.transcript
        downloadMarkdownTextFile { filename: "story.md", text } # liftEffect
    , load_story: \_ -> Console.error "unimplemented: load_story"
    }

  render :: State world -> Html world
  render = runReader renderMain

type Prompt_StoryChoices_Structure =
  { choices ::
      Array
        { long_description :: String
        , short_description :: String
        }
  }

prompt_StoryChoices_schemaDef = Llm.mkSchemaDef
  "story_choices"
  { choices: Llm.mkArraySchema $ Llm.mkObjectSchema
      { long_description: Llm.mkStringSchema { description: "A long and detailed description of what the player could do next." # pure }
      , short_description: Llm.mkStringSchema { description: "A short, high-level, 1-sentence summary of the long_description." # pure }
      }
  }

type RenderM world = Reader (State world)

type Html world = H.ComponentHTML
  (Action world)
  (provider :: H.Slot (Const Void) Config Unit)
  Aff

type RenderM_Html world = RenderM world (Html world)

renderMain :: forall world. RenderM_Html world
renderMain = do
  world <- renderWorld >>= renderMainBlock { width: 350 # pure, controls: [] } "World"
  menu <- renderMenu >>= renderMainBlock { width: 250 # pure, controls: [] } "Menu"
  story <- renderStory >>= renderMainBlock
    { width: none
    , controls:
        let
          btn action title =
            HH.div
              [ HE.onClick \_ -> action
              , css do
                  tell [ "cursor: pointer", "user-select: none" ]
                  tell [ "background-color: rgb(0, 89, 95)", "color: white" ]
                  tell [ "border-radius: 0.5em", "padding: 0 0.5em" ]
              ]
              [ HH.text title ]
        in
          [ btn (inj @"save_story" unit) "Save"
          , btn (inj @"load_story" unit) "Load"
          ]
    }
    "Story"
  pure $
    HH.div
      [ css do
          tell [ "margin: auto" ]
          tell [ "width: " <> show main_width <> "px" ]
          tell [ "box-shadow: 0 0 0 1px black" ]
          tell [ "display: flex", "flex-direction: column" ]
      ]
      [ HH.div
          [ css do
              tell [ "margin: auto" ]
              tell [ "width: " <> show main_width <> "px", "height: " <> show main_height <> "px" ]
              tell [ "box-shadow: 0 0 0 1px black" ]
              tell [ "display: flex", "flex-direction: row" ]
          ]
          [ world, menu, story ]
      , HH.slot (Proxy @"provider") unit Provider.component { providerCategory: "Main", providers: Provider.providers_with_tools } (inj @"set_config" <<< pure)
      ]

renderMainBlock :: forall world. { width :: Maybe Int, controls :: Array (Html world) } -> String -> Html world -> RenderM_Html world
renderMainBlock opts title body = do
  pure $
    HH.div
      [ css do
          case opts.width of
            Just width -> do
              tell [ "flex-grow: 0", "flex-shrink: 0" ]
              tell [ "width: " <> show width <> "px" ]
            Nothing -> pure unit
          tell [ "display: flex", "flex-direction: column" ]
      ]
      [ HH.div
          [ css do
              tell [ "flex-grow: 0" ]
              tell [ "font-weight: bold", "background-color: black", "color: white" ]
              tell [ "display: flex", "flex-direction: row", "gap: 1em", "justify-content: space-between" ]
          ]
          [ HH.div
              [ css do
                  tell [ "padding: 1em" ]
              ]
              [ HH.text title ]
          , HH.div
              [ css do
                  tell [ "display: flex", "flex-direction: row", "gap: 1em", "flex-wrap: wrap" ]
                  tell [ "padding: 1em" ]
              ]
              opts.controls
          ]
      , HH.div
          [ css do
              -- tell [ "flex-grow: 1" ]
              tell [ "overflow-y: scroll" ]
          ]
          [ body ]
      ]

ref_lastStoryItem = H.RefLabel "lastStoryItem"

renderStory :: forall world. RenderM_Html world
renderStory = do
  ctx <- ask
  -- let n = ctx.transcript # length
  transcript <- ctx.transcript # traverseWithIndex renderStoryEvent
  generating_StoryEvent_status <- ctx.generating_StoryEvent_status # match
    { generating: \choice -> do
        html_choice <- choice # renderStoryChoice_short
        pure
          [ HH.div
              [ HP.classes [ HH.ClassName "column-item", HH.ClassName "story-item", HH.ClassName "choice" ] ]
              [ html_choice ]
          , HH.div
              [ HP.classes [ HH.ClassName "column-item", HH.ClassName "story-item", HH.ClassName "generating" ] ]
              [ HH.text "generating..." ]
          ]
    , error: \err -> pure
        [ HH.div
            [ HP.classes [ HH.ClassName "column-item", HH.ClassName "story-item", HH.ClassName "error" ] ]
            [ HH.text $ "error: " <> err ]
        ]
    , done: \_ -> pure []
    }
  pure $
    HH.div
      [ css do
          tell [ "padding: 1.0em" ]
          tell [ "display: flex", "flex-direction: column", "gap: 1em" ]
      ]
      ( [ transcript
        , generating_StoryEvent_status
        , [ HH.div [ HP.ref ref_lastStoryItem ] [] ]
        ] # fold
      )

renderStoryEvent :: forall world. Int -> StoryEvent world -> RenderM_Html world
renderStoryEvent _i event = do
  choice <- event.choice # renderStoryChoice_short
  pure $
    HH.div
      [ css do tell [ "display: flex", "flex-direction: column", "gap: 1em" ] ]
      [ HH.div
          [ HP.classes [ HH.ClassName "column-item", HH.ClassName "story-item", HH.ClassName "choice" ] ]
          [ choice ]
      , HH.div
          [ HP.classes [ HH.ClassName "column-item", HH.ClassName "story-item", HH.ClassName "description" ] ]
          [ HH.text event.description ]
      ]

renderStoryChoice_short :: forall world. StoryChoice world -> RenderM_Html world
renderStoryChoice_short choice = do
  pure $
    HH.div
      [ css do tell [ "font-style: italic" ] ]
      [ HH.text choice.short_description ]

renderMenu :: forall world. RenderM_Html world
renderMenu = do
  ctx <- ask
  choices <- ctx.choices # match
    { generating: \_ -> pure $
        [ HH.div
            [ HP.classes [ HH.ClassName "column-item", HH.ClassName "menu-item", HH.ClassName "generating" ] ]
            [ HH.text "generating..." ]
        ]
    , waiting_for_story: \_ -> pure $
        [ HH.div
            [ HP.classes [ HH.ClassName "column-item", HH.ClassName "menu-item", HH.ClassName "waiting_for_story" ] ]
            [ HH.text "waiting for story..." ]
        ]
    , error: \err -> pure $
        [ HH.div
            [ HP.classes [ HH.ClassName "column-item", HH.ClassName "menu-item", HH.ClassName "error" ] ]
            [ HH.text err ]
        ]
    , done: traverse \choice -> do
        html_choice <- choice # renderStoryChoice_short
        pure $
          HH.div
            [ HP.classes [ HH.ClassName "column-item", HH.ClassName "menu-item", HH.ClassName "choice" ]
            , HE.onClick $ const $ inj @"submit_choice" choice
            ]
            [ html_choice ]
    }
  pure $
    HH.div
      [ css do
          tell [ "padding: 1em" ]
          tell [ "display: flex", "flex-direction: column", "gap: 1em" ]
      ]
      choices

renderWorld :: forall world. RenderM_Html world
renderWorld = do
  ctx <- ask
  pure $
    HH.div
      []
      [ ctx.world # ctx.engine.renderWorld # HH.fromPlainHTML ]

