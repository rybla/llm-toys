module Example.DiscreteAdventure where

import Prelude

import Ai2.Llm as Llm
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (get, modify_)
import Control.Monad.Writer (tell)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson, printJsonDecodeError)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap)
import Data.Traversable (traverse)
import Data.Unfoldable (none)
import Data.Variant (Variant, match)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import Halogen (liftAff)
import Halogen as H
import Halogen.HTML (PlainHTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Utility (css, inj)

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
  , generating_StoryEvent :: Boolean
  , choices :: Variant (generating :: Unit, waiting_for_story :: Unit, ok :: Array (StoryChoice world))
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

main_width = 1000.0
main_height = 600.0

main_component ∷ forall world query output. H.Component query (Input world) output Aff
main_component = H.mkComponent { initialState, eval, render }
  where
  initialState :: _ -> State world
  initialState { engine } =
    { engine
    , world: engine.initial_world
    , transcript: engine.initial_transcript
    , generating_StoryEvent: false
    , choices: inj @"ok" engine.initial_choices
    }

  eval = H.mkEval { handleAction, handleQuery: \_query -> pure none, receive: \_input -> none, initialize: none, finalize: none }

  handleAction = match
    { submit_choice: \choice -> do
        Console.log $ "submit_choice: " <> show choice.short_description
        -- generate StoryEvent
        do
          modify_ _
            { generating_StoryEvent = true
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
            pure response.content
          modify_ \env -> env
            { transcript = env.transcript `Array.snoc` { choice, description }
            , generating_StoryEvent = false
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
            pure $ response.parsed # decodeJson
          case err_choices of
            Left err -> Console.error $ "Failed to generate choices: " <> printJsonDecodeError err
            Right { choices } ->
              modify_ _
                { choices = inj @"ok" $ choices # map \{ long_description, short_description } ->
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
      , short_description: Llm.mkStringSchema { description: "A short and high-level description of the long_description." # pure }
      }
  }

type RenderM world = Reader (State world)

type Html world = H.ComponentHTML (Action world) () Aff

type RenderM_Html world = RenderM world (Html world)

renderMain :: forall world. RenderM_Html world
renderMain = do
  world <- renderWorld >>= renderMainBlock "world"
  menu <- renderMenu >>= renderMainBlock "menu"
  story <- renderStory >>= renderMainBlock "story"
  pure $
    HH.div
      [ css do
          tell [ "margin: auto", "width: " <> show main_width <> "px" ]
          tell [ "display: flex", "flex-direction: row", "flex-wrap: wrap" ]
      ]
      [ world
      , menu
      , story
      ]

renderMainBlock :: forall world. String -> Html world -> RenderM_Html world
renderMainBlock title body = do
  pure $
    HH.div
      [ css do
          tell [ "flex-grow: 0", "flex-shrink: 0" ]
          tell [ "width: " <> show (main_width / 3.0) <> "px" ]
          tell [ "display: flex", "flex-direction: column" ]
      ]
      [ HH.div
          [ css do
              tell [ "padding: 1em" ]
              tell [ "font-weight: bold", "background-color: black", "color: white" ]
          ]
          [ HH.text title ]
      , HH.div
          [ css do tell [ "height: " <> show main_height <> "px", "overflow-y: scroll" ] ]
          [ body ]
      ]

renderStory :: forall world. RenderM_Html world
renderStory = do
  ctx <- ask
  transcript <- ctx.transcript # traverse renderStoryEvent
  generating_StoryEvent <-
    if not ctx.generating_StoryEvent then pure []
    else pure $
      [ HH.div
          [ css do
              tell [ "padding: 0.5em", "border-radius: 1em" ]
              tell [ "background-color: color-mix(in hsl, blue, transparent 80%)" ]
          ]
          [ HH.text "generating..." ]
      ]
  pure $
    HH.div
      [ css do
          tell [ "padding: 1.0em" ]
          tell [ "display: flex", "flex-direction: column", "gap: 1em" ]
      ]
      ( [ transcript
        , generating_StoryEvent
        ] # fold
      )

renderStoryEvent :: forall world. StoryEvent world -> RenderM_Html world
renderStoryEvent event = do
  choice <- event.choice # renderStoryChoice_short
  pure $
    HH.div
      [ css do tell [ "display: flex", "flex-direction: column", "gap: 1em" ] ]
      [ HH.div
          [ css do
              tell [ "padding: 0.5em", "border-radius: 1em" ]
              tell [ "background-color: color-mix(in hsl, black, transparent 80%)" ]
          ]
          [ choice ]
      , HH.div
          [ css do
              tell [ "padding: 0.5em" ]
          ]
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
    { generating: \_ -> pure $ [ HH.div [] [ HH.text "generating..." ] ]
    , waiting_for_story: \_ -> pure $ [ HH.div [] [ HH.text "waiting for story..." ] ]
    , ok: traverse \choice -> do
        html_choice <- choice # renderStoryChoice_short
        pure $
          HH.div
            [ css do
                tell [ "padding: 0.5em", "border-radius: 1em" ]
                tell [ "cursor: pointer", "user-select: none" ]
                tell [ "background-color: color-mix(in hsl, blue, transparent 80%)" ]
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

