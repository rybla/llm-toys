module Example.DiscreteAdventure where

import Prelude

import Ai2.Llm as Llm
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (get, modify_)
import Control.Monad.Writer (tell)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Unfoldable (none)
import Data.Variant (Variant, match)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utility (css, inj, todo)

--------------------------------------------------------------------------------
-- types
--------------------------------------------------------------------------------

type Input world =
  { engine :: Engine world
  }

type State world =
  { engine :: Engine world
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
  { config :: Llm.Config
  , initial_world :: world
  , renderWorld :: world -> PlainHTML
  , initial_transcript :: Array (StoryEvent world)
  , initial_choices :: Array (StoryChoice world)
  , prompt_StoryEvent :: world -> StoryChoice world -> Aff { system :: String, user :: String }
  , prompt_StoryChoices :: world -> Array (StoryEvent world) -> Aff { system :: String, user :: String }
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
    { engine
    , world: engine.initial_world
    , transcript: engine.initial_transcript
    , generating_StoryEvent_status: inj @"done" unit
    , choices: inj @"done" engine.initial_choices
    }

  eval = H.mkEval { handleAction, handleQuery: \_query -> pure none, receive: \_input -> none, initialize: none, finalize: none }

  handleAction = match
    { submit_choice: \choice -> do
        Console.log $ "submit_choice: " <> show choice.short_description
        -- generate StoryEvent
        do
          modify_ _
            { generating_StoryEvent_status = inj @"generating" choice
            , choices = inj @"waiting_for_story" unit
            }
          { engine, world, transcript } <- get
          prompt_StoryEvent <- engine.prompt_StoryEvent world choice # liftAff
          description <- do
            response <-
              Llm.generate
                { config: engine.config
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
                throwError $ Aff.error $ err
              Right { content } -> pure content
          modify_ \env -> env
            { transcript = env.transcript `Array.snoc` { choice, description }
            , generating_StoryEvent_status = inj @"done" unit
            , choices = inj @"generating" unit
            }
        -- generate StoryChoices
        do
          { engine, world, transcript } <- get
          prompt_StoryChoices <- engine.prompt_StoryChoices world transcript # liftAff
          err_choices :: Either JsonDecodeError Prompt_StoryChoices_Structure <- do
            response <-
              Llm.generate_structure
                { config: engine.config
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

type Html world = H.ComponentHTML (Action world) () Aff

type RenderM_Html world = RenderM world (Html world)

renderMain :: forall world. RenderM_Html world
renderMain = do
  world <- renderWorld >>= renderMainBlock { width: 250 # pure } "world"
  menu <- renderMenu >>= renderMainBlock { width: 250 # pure } "menu"
  story <- renderStory >>= renderMainBlock { width: none } "story"
  pure $
    HH.div
      [ css do
          tell [ "margin: auto" ]
          tell [ "width: " <> show main_width <> "px", "height: " <> show main_height <> "px" ]
          tell [ "box-shadow: 0 0 0 1px black" ]
          tell [ "display: flex", "flex-direction: row" ]
      ]
      [ world, menu, story ]

renderMainBlock :: forall world. { width :: Maybe Int } -> String -> Html world -> RenderM_Html world
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
              tell [ "padding: 1em" ]
              tell [ "font-weight: bold", "background-color: black", "color: white" ]
          ]
          [ HH.text title ]
      , HH.div
          [ css do
              -- tell [ "flex-grow: 1" ]
              tell [ "overflow-y: scroll" ]
          ]
          [ body ]
      ]

renderStory :: forall world. RenderM_Html world
renderStory = do
  ctx <- ask
  transcript <- ctx.transcript # traverse renderStoryEvent
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
        ] # fold
      )

renderStoryEvent :: forall world. StoryEvent world -> RenderM_Html world
renderStoryEvent event = do
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

