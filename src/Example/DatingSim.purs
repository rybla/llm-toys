module Example.DatingSim where

import Prelude

import Ai.Llm (mkAssistantMessage, mkSystemMessage, mkUserMessage)
import Ai.Llm as Llm
import Control.Monad.State (StateT, gets)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldMap, intercalate, null)
import Data.Lens (view, (%=))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Variant (Variant)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Utility (for_count, inj, onLens', prop, todo)

apiKey = ""
baseURL = ""
model = ""

type Env =
  { player :: Person
  , world :: WorldState
  }

type Person =
  { name :: String
  , physicality :: Qualia
  , personality :: Qualia
  , traits :: PersonTraits
  }

type PersonTraits =
  { charm :: Number
  , empathy :: Number
  , confidence :: Number
  , intelligence :: Number
  , wisdom :: Number
  }

type WorldState =
  { stage ::
      Variant
        ( filtering :: FilteringState
        , story :: StoryState
        )
  }

type FilteringState = {}

type StoryState =
  { arc :: StoryArc
  , arc_step_index :: Int
  , transcript :: Array { prompt :: Qualia, reply :: Qualia }
  }

type StoryChoice =
  { description :: Qualia
  , traits_diff :: PersonTraits
  }

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: input -> Env
  initialState _ =
    { player:
        { name: "Ash"
        , physicality: "Ash has blue eyes, long black hair, and a thin, athletic build. They may be interested in sports or exercise as well." # wrap
        , personality: "In addition to their physical features, Ash is intelligent, kind-hearted, and independent. They enjoy spending time outdoors, traveling, and exploring new things." # wrap
        , traits:
            { charm: 0.5
            , empathy: 0.5
            , confidence: 0.5
            , intelligence: 0.5
            , wisdom: 0.5
            }
        }
    , world:
        { stage: inj @"story"
            { arc: todo "StoryArc"
            , arc_step_index: zero
            , transcript: mempty
            }
        }
    }

  eval = H.mkEval H.defaultEval

  render state =
    HH.div
      [ HP.style "padding: 1em; display: flex; flex-direction: column; gap: 1em;" ]
      [ HH.div
          [ HP.style "flex-grow: 0; flex-shrink: 0;" ]
          [ HH.table
              [ HP.style "border-collapse: collapse" ]
              let
                key_style = "vertical-align: top; text-align: right; background-color: black; color: white; padding: 0.5em; border: 1px solid black;"
                val_style = "vertical-align: top; text-align: left; padding: 0.5em; border: 1px solid black;"
              in
                [ HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "name: " ], HH.td [ HP.style $ val_style ] [ HH.text state.player.name ] ]
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "personality: " ], HH.td [ HP.style $ val_style ] [ HH.text $ state.player.personality # unwrap ] ]
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "physicality: " ], HH.td [ HP.style $ val_style ] [ HH.text $ state.player.physicality # unwrap ] ]
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "charm: " ], HH.td [ HP.style $ val_style ] [ HH.text $ show state.player.traits.charm ] ]
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "empathy: " ], HH.td [ HP.style $ val_style ] [ HH.text $ show state.player.traits.empathy ] ]
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "confidence: " ], HH.td [ HP.style $ val_style ] [ HH.text $ show state.player.traits.confidence ] ]
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "wisdom: " ], HH.td [ HP.style $ val_style ] [ HH.text $ show state.player.traits.wisdom ] ]
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "intelligence: " ], HH.td [ HP.style $ val_style ] [ HH.text $ show state.player.traits.intelligence ] ]
                ]
          ]
      , HH.div
          [ HP.style "flex-grow: 1; flex-shrink: 0;" ]
          [ HH.div
              [ HP.style "padding: 0.5em; box-shadow: 0 0 0 1px black; height: 600px;" ]
              [ HH.text "<game>" ]
          ]
      ]

--------------------------------------------------------------------------------
-- StoryArc
--------------------------------------------------------------------------------

-- An StoryArc is a high-level description of what will happen over the course of the story.
-- StoryArc terms should be human-procured.
-- The arc_steps define a possibility tree for how the story can progress.
-- Each step has a trigger that can be activated by something currently happening, which will decide between multiple branches of the tree.
-- The trigger qualia is evaluated by the LLM of course, taking into account the current situation.
-- Note the trigger qualia will probably take significantly into account the personalities of the player and the partner.
-- There is implicitly a max amount of time that can be spent on any one step, so if no trigger is satisfied within that amount of time then a step will be chosen randomly.
newtype StoryArc = StoryArc
  { name :: String
  , premise :: Qualia
  , intro :: Qualia
  , arc_steps :: List StoryArcStep
  }

type StoryArcStep =
  { trigger :: Qualia
  , description :: Qualia
  }

--------------------------------------------------------------------------------
-- update
--------------------------------------------------------------------------------

summarizeStory :: forall m. MonadAff m => StoryState -> m Qualia
summarizeStory = todo "summarizeStory"

applyStoryChoiceToPersonTraits :: StoryChoice -> PersonTraits -> PersonTraits
applyStoryChoiceToPersonTraits = todo "applyStoryChoiceToPersonTraits"

updateStory :: forall m. MonadAff m => StoryChoice -> StateT Env m (List StoryChoice)
updateStory choice = do
  story <- gets $ view $ prop @"world" <<< prop @"stage" <<< onLens' @"story"

  -- TODO: is this needed?
  -- summary <- summarizeStory story

  -- apply choice to player's traits
  prop @"player" <<< prop @"traits" %= applyStoryChoiceToPersonTraits choice

  -- write next portion of story
  result <-
    Llm.generate
      { apiKey
      , baseURL
      , model
      , messages:
          [ [ mkSystemMessage $
                [ "You are a professional flash fiction writer."
                , "You are currently writing a flash fiction story in collaboration with the user."
                , "The way this works is that the user will you a high-level prompt for what should happen next in the story, and you should reply with a single paragraph narrating just this next portion of the story."
                , "Make sure that your writing follows the user's prompt at a high level, but also make sure to flush out your writing with all the details of a good story."
                ]
                  # intercalate " "
            ]
          , story.transcript # foldMap \x ->
              [ mkUserMessage $ x.prompt # unwrap
              , mkAssistantMessage $ x.reply # unwrap
              ]
          , [ mkUserMessage $ choice.description # unwrap ]
          ] # fold
      , tools: mempty
      , tool_choice: wrap $ inj @"none" unit
      }
      # liftAff
  reply <- case result of
    Left err -> liftAff $ throwError $ Aff.error $ "generation error: " <> err
    Right { tool_calls } | not $ null tool_calls -> liftAff $ throwError $ Aff.error $ "generation error: shouldn't be using tools: " <> show tool_calls
    Right { content: Nothing } -> liftAff $ throwError $ Aff.error $ "generation error: no content"
    Right { content: Just reply } -> pure reply
  prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"transcript" %=
    (_ `Array.snoc` { prompt: choice.description, reply: reply # wrap })

  -- generate next choices

  let n_choices = 3
  for_count n_choices do
    -- sample a random fluctuation of traits
    -- 
    pure unit

  todo "updateStory"

--------------------------------------------------------------------------------
-- Qualia
--------------------------------------------------------------------------------

-- A Qualia is a string that is intended to be interpreted semantically by an LLM.
newtype Qualia = Qualia String

derive instance Newtype Qualia _

