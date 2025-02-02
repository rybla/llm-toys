module Example.DatingSim where

import Prelude

import Ai.Llm (GenerateConfig, mkAssistantMessage, mkSystemMessage, mkUserMessage)
import Ai.Llm as Llm
import Control.Monad.State (class MonadState, get, gets)
import Data.Array as Array
import Data.Foldable (fold, foldMap, foldr, intercalate, null)
import Data.Lens (view, (%=), (.=))
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Number (abs)
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class.Console as Console
import Effect.Random as Random
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utility (bug, combinations, forM_count, impossible, inj, onLens', prop)

--------------------------------------------------------------------------------
-- LLM config
--------------------------------------------------------------------------------

config1 :: GenerateConfig
config1 =
  { apiKey: "ollama"
  , baseURL: "http://localhost:11434/v1"
  , model: "llama3.2"
  }

--------------------------------------------------------------------------------
-- constants
--------------------------------------------------------------------------------

genre = "erotica" :: String

count_of_StoryChoices = 3 :: Int
magnitude_of_ProfileDiff = 0.1 :: Number

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
  , transcript :: Array { choice :: StoryChoice, reply :: Qualia }
  , generating_next_transcript_step :: Boolean
  , choices :: Maybe (Maybe (List StoryChoice))
  }

type StoryChoice =
  { description :: Qualia
  , diff :: ProfileDiff
  }

applyStoryChoiceToProfile :: StoryChoice -> Profile -> Profile
applyStoryChoiceToProfile choice = applyProfileDiff choice.diff

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
type StoryArc =
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

updateStory :: forall m. MonadAff m => MonadState Env m => StoryChoice -> m (List StoryChoice)
updateStory choice = do
  applyStoryChoice choice

  env <- get
  story <- gets $ view $ prop @"world" <<< prop @"stage" <<< onLens' @"story"

  prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"generating_next_transcript_step" .= true

  -- write next portion of story
  reply <-
    Llm.generate_without_tools
      { config: config1
      , messages:
          [ [ mkSystemMessage $ intercalate " " $
                [ "You are a professional flash fiction writer in the " <> genre <> " genre."
                , "You are currently writing a flash fiction story in collaboration with the user."
                , "The method of collaboration is as follows: the user will you a high-level prompt for what should happen next in the story, and you should reply with a single paragraph narrating the next portion of the story that flushes out the high-level description from the user as well as adds a little bit more of what happens immediately after the even that the user describes."
                , "Make sure that your writing follows the user's prompt at a high level, but also make sure to flush out your writing with all the details of a good story."
                , "Additionally, here are points of context about the story:"
                , "  - In the story, the main character's name is " <> env.player.name
                , "  - The story is told from " <> env.player.name <> "'s point of view."
                , "  - The premise of the story is: " <> (story.arc.premise # unwrap)
                , "  - " <> env.player.name <> "'s physicality is described as follows: " <> (env.player.physicality # unwrap)
                , "  - " <> env.player.name <> "'s personality is described as follows: " <> (env.player.personality # unwrap)
                , "  - " <> env.player.name <> " is " <> (describeProfileFieldNameAndValue 0 (env.player.profile.charm) # unwrap)
                , "  - " <> env.player.name <> " is " <> (describeProfileFieldNameAndValue 1 (env.player.profile.empathy) # unwrap)
                , "  - " <> env.player.name <> " is " <> (describeProfileFieldNameAndValue 2 (env.player.profile.confidence) # unwrap)
                , "  - " <> env.player.name <> " is " <> (describeProfileFieldNameAndValue 3 (env.player.profile.intelligence) # unwrap)
                , "  - " <> env.player.name <> " is " <> (describeProfileFieldNameAndValue 4 (env.player.profile.wisdom) # unwrap)
                ]
            ]
          , story.transcript # foldMap \x ->
              [ mkUserMessage $ x.choice.description # unwrap
              , mkAssistantMessage $ x.reply # unwrap
              ]
          , [ mkUserMessage $ choice.description # unwrap ]
          ] # fold
      }
      # liftAff

  prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"transcript" %=
    (_ `Array.snoc` { choice, reply: reply # wrap })
  prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"generating_next_transcript_step" .= false

  generateStoryChoices

applyStoryChoice :: forall m. MonadAff m => MonadState Env m => StoryChoice -> m Unit
applyStoryChoice choice = do
  prop @"player" <<< prop @"profile" %= applyStoryChoiceToProfile choice

generateStoryChoices :: forall m. MonadAff m => MonadState Env m => m (List StoryChoice)
generateStoryChoices = do
  -- TODO take into account story arc somehow
  forM_count count_of_StoryChoices \_ -> do
    diff <- generateProfileDiff magnitude_of_ProfileDiff # liftEffect
    generateStoryChoiceFromProfileDiff diff

-- would be nice not to have to feed in the _entire_ story so far to generate these choices, but clearly that's the best option in terms of quality
generateStoryChoiceFromProfileDiff
  :: forall m
   . MonadAff m
  => MonadState Env m
  => ProfileDiff
  -> m StoryChoice
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
              , "You should reply with a single SHORT, VAGUE, and HIGH-LEVEL sentence describing an example of what " <> env.player.name <> " could do next in the story which takes into account the user's description."
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
    , diff: diff
    }

describeProfileDiff :: ProfileDiff -> Qualia
describeProfileDiff (ProfileDiff diff) = wrap $ intercalate ", " $
  diff # map \(i /\ n) -> (describeProfileDiffFieldValue n # unwrap) <> " in " <> profileFieldNameAtIndex i

describeProfileDiffFieldValue :: Number -> Qualia
describeProfileDiffFieldValue n | n <= -0.1 = wrap $ "a dramatic decrease"
describeProfileDiffFieldValue n | -0.1 < n, n < 0.0 = wrap $ "a slight decrease"
describeProfileDiffFieldValue n | 0.0 < n, n < 0.1 = wrap $ "a slight increase"
describeProfileDiffFieldValue n | 0.1 <= n = wrap $ "a dramatic increase"
describeProfileDiffFieldValue _ = impossible unit

describeProfileFieldNameAndValue :: Int -> Number -> Qualia
describeProfileFieldNameAndValue i n = (describeProfileFieldValue n # unwrap) <> " in " <> profileFieldNameAtIndex i # wrap

describeProfileFieldValue :: Number -> Qualia
describeProfileFieldValue n | n <= 0.0 = "miserably deficient" # wrap
describeProfileFieldValue n | n <= 0.1 = "miserably deficient" # wrap
describeProfileFieldValue n | n <= 0.2 = "quite meager" # wrap
describeProfileFieldValue n | n <= 0.3 = "noticeably low" # wrap
describeProfileFieldValue n | n <= 0.4 = "slightly underwhelming" # wrap
describeProfileFieldValue n | n <= 0.5 = "perfectly balanced" # wrap
describeProfileFieldValue n | n <= 0.6 = "slightly strong" # wrap
describeProfileFieldValue n | n <= 0.7 = "solidly strong" # wrap
describeProfileFieldValue n | n <= 0.8 = "powerfully high" # wrap
describeProfileFieldValue n | n <= 0.9 = "borderline overwhelming" # wrap
describeProfileFieldValue _ {- n | 1.0 <= n -} = "utterly maxed out" # wrap

generateProfileDiff :: Number -> Effect ProfileDiff
generateProfileDiff magnitude = do
  trait_indices_combo_and_diffs <- do
    i1 <- Random.randomInt 0 4
    i2 <- Random.randomInt 0 3 # map \i2 -> if i2 < i1 then i2 else i2 + 1
    pure $ (i1 /\ magnitude) : (i2 /\ -magnitude) : Nil
  pure $ ProfileDiff $ trait_indices_combo_and_diffs

modifyProfileAtIndex :: Int -> (Number -> Number) -> Profile -> Profile
modifyProfileAtIndex 0 f p = p { charm = f p.charm }
modifyProfileAtIndex 1 f p = p { empathy = f p.empathy }
modifyProfileAtIndex 2 f p = p { confidence = f p.confidence }
modifyProfileAtIndex 3 f p = p { intelligence = f p.intelligence }
modifyProfileAtIndex 4 f p = p { wisdom = f p.wisdom }
modifyProfileAtIndex i _ _ = bug $ "[modifyProfileAtIndex] invalid index: " <> show i

--------------------------------------------------------------------------------
-- main_component
--------------------------------------------------------------------------------

main_component :: forall query input output. H.Component query input output Aff
main_component = H.mkComponent { initialState, eval, render }
  where
  initialState :: input -> Env
  initialState _ =
    { player:
        { name: "John"
        , physicality: "John has blue eyes, long black hair, and a thin, athletic build, and a nice cock. He may be interested in sports or exercise as well." # wrap
        , personality: "John is exploratory and open-minded and all things, from conversation topics to sex positions." # wrap
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
            { arc:
                { name: "simple example open-ended romance premise"
                , premise: "On a rain-slicked city night, John finds himself caught in the gravitational pull of the enigmatic Penelope, knowing full well that whatever comes next will test the limits of his control—and his fate." # wrap
                , intro: "The night air is thick with the scent of rain on hot pavement as John watches Penelope step out of the cab, her dress clinging to the shape of her like ink bleeding into silk. The city hums around them—neon flickering in puddles, the distant wail of a siren curling through the streets—but all he can focus on is the way she moves, deliberate and unhurried, the sharp click of her heels against the sidewalk sending a shiver up his spine. When she reaches him, her perfume—something dark, something floral, something that makes his throat tighten—wraps around him, and she tilts her chin up, eyes glinting beneath the streetlights. “Ready?” she murmurs, lips just shy of a smirk. John swallows, nods, and tells himself he’s got this—but the way her fingers graze his wrist as they start walking makes him feel like he’s already losing control." # wrap
                , arc_steps: none
                }
            , arc_step_index: zero
            , transcript: none
            , generating_next_transcript_step: false
            , choices: none
            }
        }
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction = match
    { "startStory": \_ -> do
        Console.log "[action] startStory"
        story <- gets $ view $ prop @"world" <<< prop @"stage" <<< onLens' @"story"
        prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"transcript" .=
          [ { choice:
                { description: "Write the introduction to the story." # wrap
                , diff: ProfileDiff none
                }
            , reply: story.arc.intro
            }
          ]
        prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"choices" .= pure none
        choices <- generateStoryChoices
        prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"choices" .= pure (pure choices)
        pure unit
    , "regenerateChoices": \_ -> do
        prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"choices" .= pure none
        choices <- generateStoryChoices
        prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"choices" .= pure (pure choices)
        pure unit
    , "submitChoice": \choice -> do
        Console.log "[action] submitChoice"
        prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"choices" .= pure none
        choices <- updateStory choice
        prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"choices" .= pure (pure choices)
        pure unit
    }

  render env = env.world.stage # match
    { "filtering": \_filtering -> HH.div [] [ HH.text "{{stage:filtering}}" ]
    , "story": \story ->
        HH.div
          [ HP.style "padding: 1em; display: flex; flex-direction: column; gap: 1em;" ]
          let
            key_style = "background-color: black; color: white; padding: 0.2em;"
            val_style = "padding: 0.2em;"
          in
            [ HH.div
                [ HP.style "box-shadow: 0 0 0 1px black; display: flex; flex-direction: row; gap: 1em;" ]
                [ HH.div [ HP.style "flex: 1; padding: 0.5em; display: flex; flex-direction: column; gap: 1em;" ]
                    let
                      mk_item k v = HH.div [ HP.style "display: flex; flex-direction: column; gap: 0;" ]
                        [ HH.div [ HP.style key_style ] [ HH.text k ]
                        , HH.div [ HP.style val_style ] [ HH.text v ]
                        ]
                    in
                      [ mk_item "name" env.player.name
                      , mk_item "personality" (env.player.personality # unwrap)
                      , mk_item "physicality" (env.player.physicality # unwrap)
                      ]
                {- profile -}
                , HH.div
                    [ HP.style "flex: 1; padding: 0.5em; display: flex; justify-content: center;" ]
                    let
                      mk_row k v =
                        HH.tr
                          [ HP.style "margin: 0; padding: 0;" ]
                          [ HH.td [ HP.style $ key_style <> " text-align: right; padding: 0 0.4em;" ] [ HH.text k ]
                          , HH.td [ HP.style $ val_style <> " text-align: left; padding: 0 0.4em;" ] [ HH.text $ v # describeProfileFieldValue # unwrap ]
                          ]
                    in
                      [ HH.table
                          [ HP.style "border-collapse: collapse;" ]
                          [ mk_row "charm" env.player.profile.charm
                          , mk_row "empathy" env.player.profile.empathy
                          , mk_row "confidence" env.player.profile.confidence
                          , mk_row "intelligence" env.player.profile.intelligence
                          , mk_row "wisdom" env.player.profile.wisdom
                          ]
                      ]
                ]
            , HH.div
                [ HP.style "display: flex; flex-direction: row; gap: 1em;" ]
                [ HH.div
                    [ HP.style "flex: 1; padding: 0.5em; box-shadow: 0 0 0 1px black; display: flex; flex-direction: column; gap: 1em;" ]
                    [ HH.div [] [ HH.text "Transcript:" ]
                    , HH.div
                        [ HP.style "padding: 0.5em; box-shadow: 0 0 0 1px black; height: 300px; overflow-y: scroll; display: flex; flex-direction: column; gap: 0.5em;" ] $ fold
                        [ story.transcript # foldMap \step ->
                            [ HH.div [ HP.style "padding: 0.5em; background-color: rgba(0, 0, 0, 0.2);" ] [ HH.text $ step.choice.description # unwrap ]
                            , renderProfileDiff step.choice.diff
                            , HH.div [] [ HH.text $ step.reply # unwrap ]
                            ]
                        , if not story.generating_next_transcript_step then []
                          else
                            [ HH.text $ "generating..." ]

                        ]
                    ]
                , HH.div
                    [ HP.style "flex: 1; padding: 0.5em; box-shadow: 0 0 0 1px black; display: flex; flex-direction: column; gap: 1em;" ]
                    [ HH.div [] [ HH.text "Choices:" ]
                    , HH.div
                        [ HP.style "max-height: 300px; overflow-y: scroll; display: flex; flex-direction: column; gap: 1em; padding: 0.1em;" ] $
                        case story.choices of
                          Nothing ->
                            [ HH.div
                                []
                                [ HH.button [ HE.onClick $ const $ inj @"startStory" unit ] [ HH.text "start" ] ]
                            ]
                          Just Nothing ->
                            [ HH.div
                                []
                                [ HH.text "generating..." ]
                            ]
                          Just (Just choices) ->
                            fold
                              [ choices # Array.fromFoldable # map \choice -> HH.div
                                  [ HP.style "padding: 0.5em; box-shadow: 0 0 0 1px black; cursor: pointer;"
                                  , HE.onClick $ const $ inj @"submitChoice" choice
                                  ]
                                  [ renderStoryChoice choice ]
                              , [ HH.button [ HE.onClick $ const $ inj @"regenerateChoices" unit ] [ HH.text "regenerate" ] ]
                              ]
                    ]
                ]
            ]
    }

renderStoryChoice :: forall w i. StoryChoice -> HTML w i
renderStoryChoice choice =
  HH.div
    [ HP.style "display: flex; flex-direction: column; gap: 0.5em;" ]
    [ renderProfileDiff choice.diff
    , HH.text $ choice.description # unwrap
    ]

renderProfileDiff :: forall w i. ProfileDiff -> HTML w i
renderProfileDiff (ProfileDiff ds) =
  HH.div
    [ HP.style "display: flex; flex-direction: row; gap: 0.5em; font-size: 0.8em;" ]
    if not (null ds) then
      ds # Array.fromFoldable # map \(i /\ dx) ->
        HH.div
          [ HP.style $
              fold
                [ "padding: 0.2em; "
                , if dx < 0.0 then
                    "background-color: rgba(255, 0, 0, 0.5); "
                  else if dx > 0.0 then
                    "background-color: rgba(0, 255, 0, 0.5); "
                  else ""
                ]
          ]
          [ HH.text $ fold
              -- [ if dx < 0.0 then "-" else if dx > 0.0 then "+" else ""
              -- , show $ abs dx
              -- , " "
              -- , profileFieldNameAtIndex i
              -- ]
              [ profileFieldNameAtIndex i ]
          ]
    else
      [ HH.div
          [ HP.style "padding: 0.2em; background-color: rgba(0, 0, 255, 0.5);" ]
          [ HH.text "neutral" ]
      ]

--------------------------------------------------------------------------------
-- Qualia
--------------------------------------------------------------------------------

-- A Qualia is a string that is intended to be interpreted semantically by an LLM.
newtype Qualia = Qualia String

derive instance Newtype Qualia _
