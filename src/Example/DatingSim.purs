module Example.DatingSim where

import Prelude

import Ai.Llm (GenerateConfig, mkAssistantMessage, mkSystemMessage, mkUserMessage)
import Ai.Llm as Llm
import Control.Bind (bindFlipped)
import Control.Monad.State (get, gets)
import Data.Array as Array
import Data.Const (Const)
import Data.Foldable (fold, foldMap, foldr, intercalate, null)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (view, (%=), (.=))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe', maybe)
import Data.Newtype (class Newtype, over, unwrap, wrap)
import Data.Tuple (fst, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (none)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Effect.Class.Console as Console
import Effect.Random as Random
import Halogen (HalogenM, liftEffect)
import Halogen as H
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))
import Utility (bug, forM_count, impossible, inj, onLens', prop, scrollIntoView)
import Web.HTML.HTMLElement as HTMLElement

--------------------------------------------------------------------------------
-- LLM config
--------------------------------------------------------------------------------

config1 :: GenerateConfig
config1 =
  { apiKey: "ollama"
  , baseURL: "http://localhost:11434/v1"
  , model: "llama3.2"
  -- , model: "llama2-uncensored:latest"
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

newtype Profile = Profile (Map String Number)

derive instance Newtype Profile _

default_Profile :: Profile
default_Profile = wrap $ Map.fromFoldable
  [ "arrogance" /\ 0.5
  , "emotiveness" /\ 0.5
  , "thoughtfulness" /\ 0.5
  , "sincereness" /\ 0.5
  , "sophistication" /\ 0.5
  ]

profileFieldNameAtIndex :: Int -> String
profileFieldNameAtIndex 0 = "arrogance"
profileFieldNameAtIndex 1 = "emotiveness"
profileFieldNameAtIndex 2 = "thoughtfulness"
profileFieldNameAtIndex 3 = "sincereness"
profileFieldNameAtIndex 4 = "sophistication"
profileFieldNameAtIndex i = bug $ "[profileFieldNameAtIndex] invalid index: " <> show i

newtype ProfileDiff = ProfileDiff (Array (String /\ Number))

applyProfileDiff :: ProfileDiff -> Profile -> Profile
applyProfileDiff (ProfileDiff diff) p = foldr (\(k /\ dx) -> over Profile $ Map.update ((_ + dx) >>> pure) k) p diff

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

updateStory :: StoryChoice -> M (List StoryChoice)
updateStory choice = do
  applyStoryChoice choice

  env <- get
  story <- gets $ view $ prop @"world" <<< prop @"stage" <<< onLens' @"story"
  basic_preamble <- getBasicPreamble
  story_preamble <- getStoryPreamble

  prop @"world" <<< prop @"stage" <<< onLens' @"story" <<< prop @"generating_next_transcript_step" .= true

  -- write next portion of story
  reply <-
    Llm.generate_basic
      { config: config1
      , messages:
          [ [ mkSystemMessage $ intercalate "\n"
                [ basic_preamble # unwrap
                , ""
                , story_preamble # unwrap
                , ""
                , intercalate " "
                    [ "You are currently writing a flash fiction story in collaboration with the user."
                    , "The method of collaboration is this: you and the user will take turns, where the user provides a prompt for what should happen next in the story, and you should reply with a single paragraph narrating in prose the next portion of the story that matches the user's prompt."
                    , "It is critical your writing respects the user's prompt while also flushing out the prose with all the details that make a good story to read."
                    ]
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

applyStoryChoice :: StoryChoice -> M Unit
applyStoryChoice choice = do
  prop @"player" <<< prop @"profile" %= applyStoryChoiceToProfile choice

generateStoryChoices :: M (List StoryChoice)
generateStoryChoices = do
  -- TODO take into account story arc somehow
  forM_count count_of_StoryChoices \_ -> do
    diff <- sampleProfileDiff magnitude_of_ProfileDiff # liftEffect
    generateStoryChoiceFromProfileDiff diff

-- would be nice not to have to feed in the _entire_ story so far to generate these choices, but clearly that's the best option in terms of quality
generateStoryChoiceFromProfileDiff :: ProfileDiff -> M StoryChoice
generateStoryChoiceFromProfileDiff diff = do
  env <- get
  story <- gets $ view $ prop @"world" <<< prop @"stage" <<< onLens' @"story"
  basic_preamble <- getBasicPreamble
  story_preamble <- getStoryPreamble
  reply <-
    Llm.generate_basic
      { config: config1
      , messages:
          [ mkSystemMessage $ intercalate "\n" $
              [ basic_preamble # unwrap
              , ""
              , story_preamble # unwrap
              , ""
              , intercalate " "
                  [ "You are currently assisting the user in creative brainstorming for a story they are writing."
                  , "You should help the user in the following way: the user will provide a high-level description of how they want the immediate next part of the story to reflect upon " <> env.player.name <> ", and you should reply with a single high-level, vague, and concise sentence describing what " <> env.player.name <> " could do next that would demonstrate what the user wants to be reflected."
                  , "It is critically important that you are creative in making the story interesting and thematic while also taking into account the user's instruction."
                  , "MAKE SURE your reply is a SINGLE SHORT HIGH-LEVEL SENTENCE."
                  ]
              , ""
              , "For context, the following is the story as written so far, right up to the point where the user is considering what should happen next:"
              , ""
              , ""
              , story.transcript # map (_.reply >>> unwrap) # intercalate "\n\n"
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

sampleProfileDiff :: Number -> Effect ProfileDiff
sampleProfileDiff magnitude = do
  trait_indices_combo_and_diffs <- do
    let ks = default_Profile # unwrap # Map.toUnfoldable # map fst
    i1 <- Random.randomInt 0 4
    let k1 = ks Array.!! i1 # fromMaybe' impossible
    i2 <- Random.randomInt 0 3 # map \i2 -> if i2 < i1 then i2 else i2 + 1
    let k2 = ks Array.!! i2 # fromMaybe' impossible
    pure $ [ k1 /\ magnitude, k2 /\ -magnitude ]
  pure $ ProfileDiff $ trait_indices_combo_and_diffs

--------------------------------------------------------------------------------
-- describe stuff
--------------------------------------------------------------------------------

getBasicPreamble :: M Qualia
getBasicPreamble = do
  env <- get
  story <- gets $ view $ prop @"world" <<< prop @"stage" <<< onLens' @"story"
  pure $ wrap $ intercalate "\n"
    [ "You are a professional flash fiction writer and writing assistant who focuses on the " <> genre <> " genre."
    ]

getStoryPreamble :: M Qualia
getStoryPreamble = do
  env <- get
  story <- gets $ view $ prop @"world" <<< prop @"stage" <<< onLens' @"story"
  pure $ wrap $ intercalate "\n"
    [ "The following are important points of context about the story:"
    , "  - In the story, the main character's name is " <> env.player.name
    , "  - The story is told from " <> env.player.name <> "'s point of view."
    , "  - The premise of the story is: " <> (story.arc.premise # unwrap)
    , "  - " <> env.player.name <> "'s physicality is described as follows: " <> (env.player.physicality # unwrap)
    , "  - " <> env.player.name <> "'s personality is described as follows: " <> (env.player.personality # unwrap)
    , intercalate "\n" $ env.player.profile # unwrap # (Map.toUnfoldable :: _ -> Array _) # map \(k /\ x) ->
        "  - " <> (describeProfileFieldNameAndValue k x # unwrap)
    ]

describeProfileDiff :: ProfileDiff -> Qualia
describeProfileDiff (ProfileDiff diff) = wrap $ intercalate ", and " $
  diff # map \(k /\ n) -> (describeProfileDiffFieldValue n # unwrap) <> " in " <> k

describeProfileDiffFieldValue :: Number -> Qualia
describeProfileDiffFieldValue n | n <= -0.1 = wrap $ "a dramatic decrease"
describeProfileDiffFieldValue n | -0.1 < n, n < 0.0 = wrap $ "a slight decrease"
describeProfileDiffFieldValue n | 0.0 < n, n < 0.1 = wrap $ "a slight increase"
describeProfileDiffFieldValue n | 0.1 <= n = wrap $ "a dramatic increase"
describeProfileDiffFieldValue _ = impossible unit

describeProfileFieldNameAndValue :: String -> Number -> Qualia
describeProfileFieldNameAndValue k n = (describeProfileFieldValue n # unwrap) <> " in " <> k # wrap

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

--------------------------------------------------------------------------------
-- main_component
--------------------------------------------------------------------------------

type M =
  HalogenM
    Env
    ( Variant
        ( "startStory" :: Unit
        , "regenerateChoices" :: Unit
        , "submitChoice" :: StoryChoice
        )
    )
    (story_step :: H.Slot (Const Void) Void Int)
    Void
    Aff

main_component :: forall query input. H.Component query input Void Aff
main_component = H.mkComponent { initialState, eval, render }
  where
  initialState :: input -> Env
  initialState _ =
    { player:
        { name: "John"
        , physicality: "John is an athletic man with a muscular build and short brown hair that often falls into his eyes." # wrap
        , personality: "John is known for being laid back, but also has an infectious laugh and a penchant for trying new things." # wrap
        , profile: default_Profile
        }
    , world:
        { stage: inj @"story"
            { arc:
                { name: "simple example open-ended romance premise"
                , premise: "On a rain-slicked city night, John finds himself caught in the gravitational pull of the enigmatic Penelope, knowing full well that whatever comes next will test the limits of his control—and his fate." # wrap
                , intro: "The night air is thick with the scent of rain on hot pavement as John watches Penelope step out of the cab, her dress clinging to the shape of her like ink bleeding into silk. The city hums around them—neon flickering in puddles, the distant wail of a siren curling through the streets—but all he can focus on is the way she moves, deliberate and unhurried, the sharp click of her heels against the sidewalk sending a shiver up his spine. When she reaches him, her perfume—something dark, something floral, something that makes his throat tighten—wraps around him, and she tilts her chin up, eyes glinting beneath the streetlights. “Ready?” she murmurs, lips just shy of a smirk. John swallows, nods, and tells himself he's got this—but the way her fingers graze his wrist as they start walking makes him feel like he's already losing control." # wrap
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
                          (env.player.profile # unwrap # Map.toUnfoldable # map (uncurry mk_row))
                      ]
                ]
            , HH.div
                [ HP.style "display: flex; flex-direction: row; gap: 1em;" ]
                [ HH.div
                    [ HP.style "flex: 1; padding: 0.5em; box-shadow: 0 0 0 1px black; display: flex; flex-direction: column; gap: 1em;" ]
                    [ HH.div [] [ HH.text "Transcript:" ]
                    , HH.div
                        [ HP.style "padding: 0.5em; box-shadow: 0 0 0 1px black; height: 300px; overflow-y: scroll; display: flex; flex-direction: column; gap: 0.5em;" ] $ fold
                        [ story.transcript # mapWithIndex \i step ->
                            HH.slot_ (Proxy @"story_step") i story_step_component
                              { body:
                                  HH.div
                                    [ HP.style "display: flex; flex-direction: column; gap: 0.5em;" ]
                                    [ HH.div [ HP.style "padding: 0.5em; background-color: rgba(0, 0, 0, 0.2);" ] [ HH.text $ step.choice.description # unwrap ]
                                    , renderProfileDiff step.choice.diff
                                    , HH.div [] [ HH.text $ step.reply # unwrap ]
                                    ]
                              }
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
      ds # Array.fromFoldable # map \(k /\ dx) ->
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
              [ k ]
          ]
    else
      [ HH.div
          [ HP.style "padding: 0.2em; background-color: rgba(0, 0, 255, 0.5);" ]
          [ HH.text "neutral" ]
      ]

story_step_component
  :: forall query output slots
   . H.Component query { body :: H.ComponentHTML (Variant (initialize :: Unit)) slots Aff } output Aff
story_step_component = H.mkComponent { initialState, eval, render }
  where
  targetRef = H.RefLabel "target"

  initialState input =
    { body: input.body
    }

  eval = H.mkEval H.defaultEval
    { initialize = pure $ inj @"initialize" unit
    , handleAction = handleAction
    }

  handleAction = match
    { "initialize": \_ -> do
        e <- H.getHTMLElementRef targetRef # bindFlipped (maybe (throwError $ Aff.error "target doesn't exist") pure)
        e # HTMLElement.toElement # scrollIntoView # liftEffect
        pure unit
    }

  render state =
    HH.div
      [ HP.ref targetRef ]
      [ state.body ]

--------------------------------------------------------------------------------
-- Qualia
--------------------------------------------------------------------------------

-- A Qualia is a string that is intended to be interpreted semantically by an LLM.
newtype Qualia = Qualia String

derive instance Newtype Qualia _
