module Example.DiscreteAdventure where

import Prelude

import Ai.Llm as Llm
import Ai.Llm.Config as Ai.Llm.Config
import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.Writer (tell)
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
  , choices ::
      Variant
        ( generating :: Unit
        , ok :: Array (StoryChoice world)
        )
  }

type Engine world =
  { initial_world :: world
  , renderWorld :: world -> PlainHTML
  , initial_transcript :: Array (StoryEvent world)
  , initial_choices :: Array (StoryChoice world)
  }

type StoryEvent world =
  { choice :: StoryChoice world
  , description :: String
  }

type StoryChoice world =
  { short_description :: String
  , full_description :: String
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

config = Ai.Llm.Config.config."openai"."gpt-4-turbo"

main_component ∷ forall world query output. H.Component query (Input world) output Aff
main_component = H.mkComponent { initialState, eval, render }
  where
  initialState :: _ -> State world
  initialState { engine } =
    { engine
    , world: engine.initial_world
    , transcript: engine.initial_transcript
    , choices: inj @"ok" engine.initial_choices
    }

  eval = H.mkEval { handleAction, handleQuery: \_query -> pure none, receive: \_input -> none, initialize: none, finalize: none }

  handleAction = match
    { submit_choice: \choice -> do
        Console.log $ "submit_choice: " <> show choice.short_description
        _ <- Llm.generate_basic { config, messages: [] } # liftAff
        pure unit
    }

  render :: State world -> Html world
  render = runReader renderMain

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
  pure $
    HH.div
      [ css do
          tell [ "padding: 1.0em" ]
          tell [ "display: flex", "flex-direction: column", "gap: 1em" ]
      ]
      transcript

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
    , ok: traverse \choice -> do
        html_choice <- choice # renderStoryChoice_short
        pure $
          HH.div
            [ css do
                tell [ "padding: 0.5em", "border-radius: 1em" ]
                tell [ "cursor: pointer", "user-select: none" ]
                tell [ "background-color: rgba(0, 0, 0, 0.2)" ]
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

