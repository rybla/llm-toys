module Example.MutableWorld.App where

import Prelude

import Ai2.Llm (Config, generate, generate_structure, mkSystemMsg, mkUserMsg)
import Ai2.Widget.Provider as Provider
import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (encodeJson, stringifyWithIndent)
import Data.Argonaut.Decode (fromJsonString)
import Data.Array (intercalate, length, take)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (fold, foldr)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens ((%=), (.=))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Example.MutableWorld.Common (Engine)
import Example.MutableWorld.World (World, WorldUpdate, applyWorldUpdate, describeWorld)
import Halogen (liftAff, liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Utility (copyToClipboard, readFromClipboard)
import Halogen.VDom.Driver as HVD
import Halogen.Widget as Widget
import Type.Prelude (Proxy(..))
import Utility (format, prop)
import Web.Event.Event as Event
import Web.HTML.HTMLTextAreaElement as HTMLTextAreaElement
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

--------------------------------------------------------------------------------

make_main :: Engine -> Effect Unit
make_main engine = HA.runHalogenAff (HVD.runUI main_component engine =<< HA.awaitBody)

--------------------------------------------------------------------------------

type Input = Engine

type State =
  { config :: Maybe Config
  , engine :: Engine
  , processing :: Boolean
  , world :: World
  , story :: Array { prompt :: String, content :: String }
  , transcript :: Array { label :: String, content :: String }
  }

data Action
  = SetConfig Config
  | SubmitPrompt PromptSource String
  | InputKeyDown PromptSource KeyboardEvent
  | Import
  | Export

data PromptSource
  = UpdateWorld_PromptSource
  | PromptStory_PromptSource

main_component :: forall query output. H.Component query Input output Aff
main_component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState engine =
    { engine
    , config: Nothing
    , processing: false
    , transcript: []
    , story: []
    , world:
        { characters: Map.empty
        , locations: Map.empty
        }
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  getConfig = get >>= \{ config } -> config # flip maybe pure do
    throwError $ Aff.error "config has not been set"

  handleAction (SetConfig config) = do
    prop @"config" .= pure config
    pure unit

  handleAction (SubmitPrompt PromptStory_PromptSource userPrompt_) = do
    config <- getConfig

    get >>= \{ processing } ->
      when processing do
        throwError $ Aff.error $ "already processing! dont submit another prompt yet"

    prop @"processing" .= true

    do
      systemMsg <- do
        pure $ mkSystemMsg $ String.trim
          """
You are a helpful fiction writing assistant.
You are currently collaborating with the user to write a novel.
The way this collaboration works is that if the user has written a portion of the story already, they will show you the latest few paragraphs in the story.
You should read these paragraphs carefully to get an idea of what's going on right now in the story.
The user will also provide a description of the current state of the story world.
Note that this description is fairly comprehensive -- it may include many details about the world that are not immediately relevant to what's going on right now in the story.
But, also note that even less-immediate details will be very useful to have in the back of your mind when considering what direction things should go in the short term, in order for the course of action to eventually lead to resolving other far-away situations in the world.
Finally, the user will provide a high-level suggestion for what they think should happen next in the story.
Consider this suggestion, and find an interpretation that makes the most sense to make the story interesting and make progress in developing the plot and the worldbuilding.

You should reply with EXACTLY 1 PARAGRAPH as the next paragraph of the story, picking up right where the user left off if they've written anything in the story already.
  """

      let userPrompt = userPrompt_ # String.trim
      promptMsg <- do
        { world, story } <- get
        pure $ mkUserMsg $ String.trim $
          """
{{story_so_far}}

So right now in the story, this is the current state of the world:

{{world}}

This is my suggestion for what should happen next in the story:

    {{prompt}}  
  """
            # format
                { story_so_far:
                    if length story == 0 then
                      "I haven't written any of the story yet. So you are helping me write the very first paragraph."
                    else
                      intercalate "\n\n"
                        [ "The following paragraphs are the most recent portion of the story I'm currently writing."
                        , intercalate "\n\n" $
                            story # take 2 # map (\{ content } -> "    " <> content)
                        ]
                , world: describeWorld world
                , prompt: userPrompt
                }

      state_backup <- get
      prop @"transcript" %=
        ( _ `Array.snoc`
            { label: "Story / User Prompt"
            , content: userPrompt
            }
        )

      result <- do
        err_msg <- lift $
          generate
            { config
            , messages:
                [ systemMsg
                , promptMsg
                ]
            }
        case err_msg of
          Left err -> do
            put state_backup
            throwError $ Aff.error $ "error when generating: " <> err
          Right msg -> pure msg

      prop @"transcript" %=
        ( _ `Array.snoc`
            { label: "Story / Model"
            , content: result.content
            }
        )

    prop @"processing" .= false

    pure unit

  handleAction (SubmitPrompt UpdateWorld_PromptSource userPrompt_) = do
    config <- getConfig

    get >>= \{ processing } ->
      when processing do
        throwError $ Aff.error $ "already processing! dont submit another prompt yet"

    prop @"processing" .= true

    let
      systemMsg = mkSystemMsg $ String.trim
        """
You are a helpful assistant for write story-related content.
You are interacting with a fictional world in collaboration with the user.
The world may start off empty, of pre-filled with some existing content from the user.
The user will give you instructions for how to update the world, by creating new content to put into the world or modifying existing content.
The idea is that these changes will reflect a story progressing in the fictional world.
You will always output in a structured form with an array of updates to apply simultaneously to the world.
Make sure to always keep the user's specific instructions in mind, but also feel free to take creative liberties and extrapolate interesting details in order to make the updates reflect an interesting sequence of events for a story!
Have fun with it!
"""

    let userPrompt = userPrompt_ # String.trim
    promptMsg <- do
      { world } <- get
      let
        prelude = describeWorld world
        prompt =
          """
Current state of the world:

{{prelude}}

User instructions: {{prompt}}
  """
            # format
                { prelude
                , prompt: userPrompt
                }
            # String.trim
      pure $ mkUserMsg prompt

    state_backup <- get
    prop @"transcript" %=
      ( _ `Array.snoc`
          { label: "Manually modify world / User Prompt"
          , content: userPrompt
          }
      )

    result <- do
      err_msg <- lift $
        generate_structure @(updates :: Array WorldUpdate)
          { config
          , name: "updates"
          , messages:
              [ systemMsg
              , promptMsg
              ]
          }
      case err_msg of
        Left err -> do
          put state_backup
          throwError $ Aff.error $ "error when generating: " <> err
        Right msg -> pure msg

    prop @"transcript" %=
      ( _ `Array.snoc`
          { label: "Manually modify world / Model Response"
          , content: result.updates # map show # intercalate "\n"
          }
      )
    prop @"world" %= \world -> foldr applyWorldUpdate world result.updates

    prop @"processing" .= false

  handleAction Export = do
    { world } <- get
    copyToClipboard (stringifyWithIndent 4 $ encodeJson world) # liftAff # void

  handleAction Import = do
    readFromClipboard # liftAff >>= case _ of
      Left _ -> pure unit
      Right s -> case fromJsonString s of
        Left _ -> pure unit
        Right world -> prop @"world" .= world

  handleAction (InputKeyDown source event) = do
    let
      key = event # KeyboardEvent.key
      cmd = (event # KeyboardEvent.ctrlKey) || (event # KeyboardEvent.metaKey)

    when (key == "Enter" && cmd) do
      el <- event
        # KeyboardEvent.toEvent
        # Event.target
        # maybe (throwError $ Aff.error "impossible") \target -> target
            # HTMLTextAreaElement.fromEventTarget
            # maybe (throwError $ Aff.error "impossible") pure
      v <- el # HTMLTextAreaElement.value # liftEffect
      HTMLTextAreaElement.setValue "" el # liftEffect
      handleAction (SubmitPrompt source v)

  render state =
    let
      n = length state.transcript

      transcript_processing_slotId = n
      transcript_bottom_slotId = n + (if state.processing then 1 else 0)
    in
      HH.div
        [ HP.classes [ H.ClassName "App" ] ]
        [ HHK.div [ HP.classes [ H.ClassName "Transcript" ] ] $ fold $
            [ state.transcript # mapWithIndex \i { label, content } ->
                Tuple (show i) $
                  HH.div [ HP.classes [ H.ClassName "Msg" ] ]
                    [ HH.div [] [ HH.text label ]
                    , HH.div [] [ HH.text content ]
                    ]
            , if not state.processing then []
              else [ Tuple (show transcript_processing_slotId) $ HH.div [] [ HH.text "processing..." ] ]
            , [ Tuple (show transcript_bottom_slotId) $ HH.slot_ (Proxy @"ScrollToMe") (show transcript_bottom_slotId) Widget.scrollToMe unit ]
            ]
        , HH.div [ HP.classes [ H.ClassName "World" ] ]
            [ HH.text $ describeWorld state.world ]
        , HH.div [ HP.classes [ H.ClassName "Toolbar" ] ]
            [ HH.button
                [ HE.onClick $ const Export ]
                [ HH.text "export" ]
            , HH.button
                [ HE.onClick $ const Import ]
                [ HH.text "import" ]
            ]
        , HH.div [ HP.classes [ H.ClassName "Prompts" ] ]
            [ HH.div [ HP.classes [ H.ClassName "PromptSectionTitle" ] ]
                [ HH.text "Manually modify world:" ]
            , HH.textarea
                [ HE.onKeyDown $ InputKeyDown UpdateWorld_PromptSource
                , HP.value "Create some locations and characters for a medieval fantasy world. Be creative!"
                ]
            , HH.div [ HP.classes [ H.ClassName "PromptSectionTitle" ] ]
                [ HH.text "Prompt next portion of story:" ]
            , HH.textarea
                [ HE.onKeyDown $ InputKeyDown PromptStory_PromptSource
                , HP.value "..."
                ]
            ]
        , HH.slot (Proxy @"provider") unit Provider.component { providerCategory: "Main", providers: Provider.providers_with_structured_output } SetConfig
        ]

