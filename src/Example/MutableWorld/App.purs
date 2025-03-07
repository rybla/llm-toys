module Example.MutableWorld.App where

import Prelude

import Ai2.Llm (AssistantMsg(..), Config, Msg(..), generate_structure, mkStructureAssistantMsg, mkSystemMsg, mkUserMsg)
import Ai2.Widget.Provider as Provider
import Control.Monad.State (get)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (encodeJson, stringify, stringifyWithIndent)
import Data.Array (length)
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
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Halogen.Widget as Widget
import Type.Prelude (Proxy(..))
import Utility (format, prop, todo)

--------------------------------------------------------------------------------

make_main :: Engine -> Effect Unit
make_main engine = HA.runHalogenAff (HVD.runUI main_component engine =<< HA.awaitBody)

--------------------------------------------------------------------------------

type Input = Engine

type State =
  { config :: Maybe Config
  , engine :: Engine
  , processing :: Boolean
  , msgs :: Array Msg
  , world :: World
  }

data Action
  = SetConfig Config
  | SubmitPrompt String
  | ExportWorld
  | ImportWorld

main_component :: forall query output. H.Component query Input output Aff
main_component = H.mkComponent { initialState, eval, render }
  where
  initialState :: Input -> State
  initialState engine =
    { engine
    , config: Nothing
    , processing: false
    , msgs:
        [ mkSystemMsg $ String.trim
            """
You are a helpful assistant for write story-related content.
You are interacting with a fictional world in collaboration with the user.
The world starts off with no content.
The user will give you instructions for how to update the world, by creating new content to put into the world or modifying existing content.
The idea is that these changes will reflect a story progressing in the fictional world.
You will always output in a structured form with an array of updates to apply simultaneously to the world.
Make sure to always keep the user's specific instructions in mind, but also feel free to take creative liberties and extrapolate interesting details in order to make the updates reflect an interesting sequence of events for a story!
Have fun with it.
"""
        ]
    , world:
        { characters: Map.empty
        , locations: Map.empty
        }
    }

  eval = H.mkEval H.defaultEval { handleAction = handleAction }

  handleAction (SetConfig config) = do
    prop @"config" .= pure config

  handleAction (SubmitPrompt prompt) = do
    config <- get >>= \{ config } -> config # flip maybe pure do
      throwError $ Aff.error "config has not been set"

    prop @"processing" .= true

    prop @"msgs" %= (_ `Array.snoc` mkUserMsg prompt)

    result <- do
      { msgs } <- get
      err_msg <- lift $
        generate_structure @(updates :: Array WorldUpdate)
          { config
          , name: "updates"
          , messages: msgs
          }
      case err_msg of
        Left err -> throwError $ Aff.error $ "error when generating: " <> err
        Right msg -> pure msg

    prop @"msgs" %= (_ `Array.snoc` mkStructureAssistantMsg (encodeJson result))

    prop @"world" %= \world -> foldr applyWorldUpdate world result.updates

    -- msg <- do
    --   { msgs } <- get
    --   err_msg <- lift $
    --     generate_structure @(value :: StringOrInt)
    --       { config
    --       , name: "StringOrInt"
    --       , messages: msgs
    --       }
    --   case err_msg of
    --     Left err -> throwError $ Aff.error $ "error when generating: " <> err
    --     Right msg -> pure msg
    -- Console.logShow { msg }

    prop @"processing" .= false

  handleAction ExportWorld = pure unit

  handleAction ImportWorld = pure unit

  render state =
    let
      length_msgs = length state.msgs

      transcript_processing_slotId = length_msgs
      transcript_bottom_slotId = length_msgs + (if state.processing then 1 else 0)

      mkPromptButton userPrompt =
        let
          prelude = describeWorld state.world
          prompt =
            """
{{prelude}}

User instructions: {{prompt}}
"""
              # format
                  { prelude
                  , prompt: String.trim userPrompt
                  }
        in
          HH.button
            [ HP.classes [ H.ClassName "PromptButton" ]
            , HE.onClick $ const $ SubmitPrompt prompt
            , HP.disabled state.processing
            ]
            [ HH.text prompt ]
    in
      HH.div
        [ HP.classes [ H.ClassName "App" ] ]
        [ HHK.div [ HP.classes [ H.ClassName "Transcript" ] ] $ fold $
            [ state.msgs # mapWithIndex \i -> case _ of
                SystemMsg msg -> Tuple (show i) $ HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "System" ] ] [ HH.div [] [ HH.text "System" ], HH.div [] [ HH.text msg.content ] ]
                UserMsg msg -> Tuple (show i) $ HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "User" ] ] [ HH.div [] [ HH.text "User" ], HH.div [] [ HH.text msg.content ] ]
                ToolMsg msg -> Tuple (show i) $ HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Tool" ] ] [ HH.div [] [ HH.text "Tool" ], HH.div [] [ HH.text msg.content ] ]
                AssistantMsg (TextAssistantMsg msg) -> Tuple (show i) $ HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Assistant", H.ClassName "Text" ] ] [ HH.div [] [ HH.text "Assistant" ], HH.div [] [ HH.text $ show msg ] ]
                AssistantMsg (ToolAssistantMsg msg) -> Tuple (show i) $ HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Assistant", H.ClassName "Tool" ] ] [ HH.div [] [ HH.text "Assistant Tool" ], HH.div [] [ HH.text $ show msg ] ]
                AssistantMsg (StructureAssistantMsg msg) -> Tuple (show i) $ HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Assistant", H.ClassName "Structure" ] ] [ HH.div [] [ HH.text "Assistant Structure" ], HH.div [] [ HH.text $ "{ parsed: {{parsed}} }" # format { parsed: stringify msg.parsed } ] ]
            , if not state.processing then []
              else [ Tuple (show transcript_processing_slotId) $ HH.div [] [ HH.text "processing..." ] ]
            , [ Tuple (show transcript_bottom_slotId) $ HH.slot_ (Proxy @"ScrollToMe") (show transcript_bottom_slotId) Widget.scrollToMe unit ]
            ]
        , HH.div [ HP.classes [ H.ClassName "World" ] ]
            [ HH.text $ stringifyWithIndent 4 $ encodeJson state.world ]
        , HH.div [ HP.classes [ H.ClassName "Toolbar" ] ]
            [ HH.button
                [ HE.onClick $ const ExportWorld ]
                [ HH.text "export world" ]
            , HH.button
                [ HE.onClick $ const ExportWorld ]
                [ HH.text "import world" ]
            ]
        , HH.div [ HP.classes [ H.ClassName "Prompts" ] ]
            [ mkPromptButton $
                """
Create some locations and characters for a medieval fantasy world. Be creative!
"""
            ]
        , HH.slot (Proxy @"provider") unit Provider.component { providerCategory: "Main", providers: Provider.providers_with_structured_output } SetConfig
        ]

