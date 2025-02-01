module Example.DatingSim where

import Prelude

import Ai.Llm (GenerateConfig, mkAssistantMessage, mkSystemMessage, mkUserMessage)
import Ai.Llm as Llm
import Control.Monad.State (StateT, get, gets)
import Data.Array as Array
import Data.Foldable (fold, foldMap, foldr, intercalate)
import Data.Lens (view, (%=))
import Data.List (List(..), (:))
import Data.Maybe (fromMaybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Random as Random
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Utility (bug, combinations, forM_count, impossible, inj, onLens', prop, todo)

--------------------------------------------------------------------------------
-- LLM config
--------------------------------------------------------------------------------

config1 :: GenerateConfig
config1 =
  { apiKey: ""
  , baseURL: ""
  , model: ""
  }

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------

genre = "erotica" :: String

count_of_StoryChoices = 3 :: Int
magnitude_of_ProfileDiff = 0.05 :: Number

--------------------------------------------------------------------------------
-- Env
--------------------------------------------------------------------------------

type Env =
  { player :: Person
  , world :: WorldState
  }

type Person =
  { name :: String
  , physicality :: Qualia
  , personality :: Qualia
  , profile :: Profile
  }

type Profile =
  { charm :: Number
  , empathy :: Number
  , confidence :: Number
  , intelligence :: Number
  , wisdom :: Number
  }

profileFieldNameAtIndex :: Int -> String
profileFieldNameAtIndex 0 = "charm"
profileFieldNameAtIndex 1 = "empathy"
profileFieldNameAtIndex 2 = "confidence"
profileFieldNameAtIndex 3 = "intelligence"
profileFieldNameAtIndex 4 = "wisdom"
profileFieldNameAtIndex i = bug $ "[profileFieldNameAtIndex] invalid index: " <> show i

newtype ProfileDiff = ProfileDiff (List (Int /\ Number))

applyProfileDiff :: ProfileDiff -> Profile -> Profile
applyProfileDiff (ProfileDiff diff) p = foldr (\(i /\ dx) -> modifyProfileAtIndex i (_ + dx)) p diff

derive instance Newtype ProfileDiff _

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
  , profileDiff :: ProfileDiff
  }

applyStoryChoiceToProfile :: StoryChoice -> Profile -> Profile
applyStoryChoiceToProfile = todo "applyStoryChoiceToProfile"

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
-- loopStory
--------------------------------------------------------------------------------

loopStory :: forall m. MonadAff m => StateT Env m Unit
loopStory = do
  pure unit

--------------------------------------------------------------------------------
-- update
--------------------------------------------------------------------------------

applyStoryChoice :: forall m. MonadAff m => StoryChoice -> StateT Env m Unit
applyStoryChoice choice = do
  prop @"player" <<< prop @"profile" %= applyStoryChoiceToProfile choice

updateStory :: forall m. MonadAff m => StoryChoice -> StateT Env m (List StoryChoice)
updateStory choice = do
  story <- gets $ view $ prop @"world" <<< prop @"stage" <<< onLens' @"story"

  -- apply choice to player's profile
  prop @"player" <<< prop @"profile" %= applyStoryChoiceToProfile choice

  -- write next portion of story
  reply <-
    Llm.generate_without_tools
      { config: config1
      , messages:
          [ [ mkSystemMessage $ intercalate " " $
                [ "You are a professional flash fiction writer in the " <> genre <> " genre."
                , "You are currently writing a flash fiction story in collaboration with the user."
                , "The way this works is that the user will you a high-level prompt for what should happen next in the story, and you should reply with a single paragraph narrating just this next portion of the story."
                , "Make sure that your writing follows the user's prompt at a high level, but also make sure to flush out your writing with all the details of a good story."
                ]
            ]
          , story.transcript # foldMap \x ->
              [ mkUserMessage $ x.prompt # unwrap
              , mkAssistantMessage $ x.reply # unwrap
              ]
          , [ mkUserMessage $ choice.description # unwrap ]
          ] # fold
      }
      # liftAff
  prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"transcript" %=
    (_ `Array.snoc` { prompt: choice.description, reply: reply # wrap })

  generateStoryChoices

generateStoryChoices :: forall m. MonadAff m => StateT Env m (List StoryChoice)
generateStoryChoices = do
  -- TODO take into account story arc somehow
  forM_count count_of_StoryChoices \_ -> do
    profileDiff <- generateProfileDiff magnitude_of_ProfileDiff # liftEffect
    generateStoryChoiceFromProfileDiff profileDiff

-- would be nice not to have to feed in the _entire_ story so far to generate these choices, but clearly that's the best option in terms of quality
generateStoryChoiceFromProfileDiff
  :: forall m
   . MonadAff m
  => ProfileDiff
  -> StateT Env m StoryChoice
generateStoryChoiceFromProfileDiff diff = do
  env <- get
  story <- gets $ view $ prop @"world" <<< prop @"stage" <<< onLens' @"story"
  reply <-
    Llm.generate_without_tools
      { config: config1
      , messages:
          [ mkSystemMessage $ intercalate " " $
              [ "You are a professional creative writing assisstant who is helping the user to write a flash fiction story in the " <> genre <> " genre."
              , "In the story, the main character's name is " <> env.player.name
              , "The story is told from " <> env.player.name <> "'s point of view."
              , "The following is the story so far:"
              , ""
              , story.transcript # map (_.reply >>> unwrap >>> ("    " <> _)) # intercalate "\n"
              , ""
              , "The next event of the story depends on what " <> env.player.name <> " does next."
              , "The user will provide description of how what " <> env.player.name <> " does next should reflect on their character."
              , "You should reply with a single sentence describing at a high level an example of what " <> env.player.name <> " could do next in the story which takes into account the user's description."
              , "It is critically important that you are creative and help make the story interesting while also taking into account the user's instructions."
              ]
          , mkUserMessage $ intercalate " " $
              [ env.player.name <> "'s next action should reflect " <> (describeProfileDiff diff # unwrap) <> "."
              ]
          ]
      }
      # liftAff
  pure
    { description: reply # wrap
    , profileDiff: diff
    }

describeProfileDiff :: ProfileDiff -> Qualia
describeProfileDiff (ProfileDiff diff) = wrap $ intercalate ", " $
  diff # map \(i /\ n) -> (describeProfileFieldValueDiff n # unwrap) <> " in " <> profileFieldNameAtIndex i

describeProfileFieldValueDiff :: Number -> Qualia
describeProfileFieldValueDiff n | n <= -0.1 = wrap $ "a dramatic decrease"
describeProfileFieldValueDiff n | -0.1 < n, n < 0.0 = wrap $ "a slight decrease"
describeProfileFieldValueDiff n | 0.0 < n, n < 0.1 = wrap $ "a slight increase"
describeProfileFieldValueDiff n | 0.1 <= n = wrap $ "a dramatic increase"
describeProfileFieldValueDiff _ = impossible unit

generateProfileDiff :: Number -> Effect ProfileDiff
generateProfileDiff magnitude = do
  trait_indices_combo_and_diffs <- do
    trait_indices_combo <- do
      trait_combo_index <- Random.randomInt 0 (combinations_of_trait_indices # Array.length)
      pure $ combinations_of_trait_indices Array.!! trait_combo_index # fromMaybe' impossible
    trait_indices_combo # traverse \i -> do
      polarity <- Random.randomBool
      let diff = magnitude * (if polarity then 1.0 else -1.0)
      pure $ i /\ diff
  pure $ ProfileDiff $ trait_indices_combo_and_diffs

modifyProfileAtIndex :: Int -> (Number -> Number) -> Profile -> Profile
modifyProfileAtIndex 0 f p = p { charm = f p.charm }
modifyProfileAtIndex 1 f p = p { empathy = f p.empathy }
modifyProfileAtIndex 2 f p = p { confidence = f p.confidence }
modifyProfileAtIndex 3 f p = p { intelligence = f p.intelligence }
modifyProfileAtIndex 4 f p = p { wisdom = f p.wisdom }
modifyProfileAtIndex i _ _ = bug $ "[modifyProfileAtIndex] invalid index: " <> show i

combinations_of_trait_indices :: Array (List Int)
combinations_of_trait_indices = combinations 5 (0 : 1 : 2 : 3 : 4 : Nil) # Array.fromFoldable

--------------------------------------------------------------------------------
-- component
--------------------------------------------------------------------------------

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: input -> Env
  initialState _ =
    { player:
        { name: "Ash"
        , physicality: "Ash has blue eyes, long black hair, and a thin, athletic build. They may be interested in sports or exercise as well." # wrap
        , personality: "In addition to their physical features, Ash is intelligent, kind-hearted, and independent. They enjoy spending time outdoors, traveling, and exploring new things." # wrap
        , profile:
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
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "charm: " ], HH.td [ HP.style $ val_style ] [ HH.text $ show state.player.profile.charm ] ]
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "empathy: " ], HH.td [ HP.style $ val_style ] [ HH.text $ show state.player.profile.empathy ] ]
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "confidence: " ], HH.td [ HP.style $ val_style ] [ HH.text $ show state.player.profile.confidence ] ]
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "wisdom: " ], HH.td [ HP.style $ val_style ] [ HH.text $ show state.player.profile.wisdom ] ]
                , HH.tr_ [ HH.td [ HP.style $ key_style ] [ HH.text "intelligence: " ], HH.td [ HP.style $ val_style ] [ HH.text $ show state.player.profile.intelligence ] ]
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
-- Qualia
--------------------------------------------------------------------------------

-- A Qualia is a string that is intended to be interpreted semantically by an LLM.
newtype Qualia = Qualia String

derive instance Newtype Qualia _
