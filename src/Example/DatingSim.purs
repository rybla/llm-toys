module Example.DatingSim where

import Prelude

import Control.Monad.State (StateT)
import Data.List (List)
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Tree (Tree)
import Data.Variant (Variant)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Utility (inj, todo)

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

type Env =
  { player :: Person
  , world :: WorldState
  }

type WorldState =
  { status ::
      Variant
        ( filtering :: FilteringState
        , story :: StoryState
        )
  }

type FilteringState = {}

type StoryState =
  { arc :: StoryArc
  , arc_step_index :: Int
  , events :: List Qualia
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
        { status: inj @"story"
            { arc: todo "StoryArc"
            , arc_step_index: zero
            , events: mempty
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

summarizeStory :: forall m. MonadAff m => StateT StoryState m Qualia
summarizeStory = todo "summarizeStory"

updateStory :: forall m. MonadAff m => StoryChoice -> StateT StoryState m (List StoryChoice)
updateStory choice = do
  -- summary <- summarizeStory
  -- apply choice to player traits
  -- generate: flush out event that is the choice enacted and the immediate consequences of it
  -- generate: next choices, partly based on random fluctuation of a few traits
  todo "updateStory"

--------------------------------------------------------------------------------
-- Qualia
--------------------------------------------------------------------------------

-- A Qualia is a string that is intended to be interpreted semantically by an LLM.
newtype Qualia = Qualia String

derive instance Newtype Qualia _

