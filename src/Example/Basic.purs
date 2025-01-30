module Example.Basic where

import Prelude

import Ai.Llm (ToolChoice(..))
import Ai.Llm as Llm
import Control.Monad.State (get, modify_)
import Data.Argonaut.Decode (fromJsonString)
import Data.Argonaut.Encode (toJsonString)
import Data.Either (Either(..), fromRight')
import Data.Lens ((%=), (.=))
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (none)
import Data.Variant (Variant, case_)
import Effect.Aff (Aff, throwError)
import Effect.Aff as Aff
import Effect.Aff.Class (liftAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Utility (impossible, inj, on, prop)
import Web.HTML.HTMLTextAreaElement as HTMLTextAreaElement

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  inputRefLabel = H.RefLabel "input"

  initialState _ =
    { output: ""
    , status: inj @"idle" unit :: Variant (idle :: Unit, working :: Unit)
    , model: "command-r7b"
    , tool_choice: ToolChoice (inj @"auto" unit)
    }

  eval = H.mkEval H.defaultEval
    { handleAction = case_
        # on @"submit"
            ( \_ -> do
                input_elem <- H.getHTMLElementRef inputRefLabel >>= maybe (throwError (Aff.error "impossible")) pure
                input <- input_elem # HTMLTextAreaElement.fromHTMLElement # maybe (throwError (Aff.error "impossible")) (HTMLTextAreaElement.value >>> liftEffect)
                prop @"status" .= inj @"working" unit
                state <- get
                result <-
                  Llm.generate
                    { apiKey: ""
                    , baseURL: "http://localhost:11434/v1"
                    , model: state.model
                    , messages: [ wrap $ inj @"user" { name: none, content: input } ]
                    , tools:
                        [ wrap $ inj @"function"
                            { name: "set_object_color"
                            , description: "Sets the object's color."
                            , parameters: wrap $ Map.fromFoldable
                                [ Tuple "color" $ wrap $ inj @"string" { description: "The color to set the object's color to." }
                                ]
                            }
                        , wrap $ inj @"function"
                            { name: "set_object_position"
                            , description: "Sets the object's position"
                            , parameters: wrap $ Map.fromFoldable
                                [ Tuple "x" $ wrap $ inj @"number" { description: "The new x-coordinate of the object." }
                                , Tuple "y" $ wrap $ inj @"number" { description: "The new y-coordinate of the object." }
                                ]
                            }
                        ]
                    , tool_choice: state.tool_choice
                    }
                    # liftAff
                case result of
                  Left err -> do
                    prop @"output" .= "error: " <> err
                  Right msg -> do
                    prop @"output" .= (msg.content # fromMaybe "undefined")
                    msg.tool_calls # traverse_ \tool_call ->
                      prop @"output" %= (_ <> ("\n • " <> show (unwrap tool_call)))
                prop @"status" .= inj @"idle" unit
            )
        # on @"modify" \f -> modify_ f
    }

  render state =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 1.0em;" ]
      [ HH.textarea
          [ HP.ref inputRefLabel
          , HP.value $ "Set the object's color to red."
          ]
      , HH.div
          [ HP.style "display: flex; flex-direction: row; gap: 0.5em;" ]
          [ HH.div [] [ HH.text "model" ]
          , HH.select
              [ HE.onValueChange \model -> inj @"modify" _ { model = model } ]
              [ HH.option [ HP.value "command-r7b" ] [ HH.text "command-r7b" ]
              , HH.option [ HP.value "phi4" ] [ HH.text "phi4" ]
              , HH.option [ HP.value "llama3.2" ] [ HH.text "llama3.2" ]
              ]
          ]
      , HH.div
          [ HP.style "display: flex; flex-direction: row; gap: 0.5em;" ]
          [ HH.div [] [ HH.text "tool_choice" ]
          , HH.select
              [ HE.onValueChange \tool_choice -> inj @"modify" _ { tool_choice = tool_choice # fromJsonString # fromRight' impossible } ]
              [ HH.option [ HP.value $ inj @"auto" unit # ToolChoice # toJsonString ] [ HH.text "auto" ]
              , HH.option [ HP.value $ inj @"none" unit # ToolChoice # toJsonString ] [ HH.text "none" ]
              , HH.option [ HP.value $ inj @"required" unit # ToolChoice # toJsonString ] [ HH.text "required" ]
              , HH.option [ HP.value $ inj @"named" "set_object_color" # ToolChoice # toJsonString ] [ HH.text "named:set_object_color" ]
              , HH.option [ HP.value $ inj @"named" "set_object_position" # ToolChoice # toJsonString ] [ HH.text "named:set_object_position" ]
              ]
          ]
      , HH.button
          [ HE.onClick (const (inj @"submit") unit) ]
          [ HH.text "submit" ]
      , HH.div
          [ HP.style "whitespace: pre;" ]
          [ state.status #
              ( case_
                  # on @"idle" (\_ -> HH.text state.output)
                  # on @"working" (\_ -> HH.text "working...")
              )
          ]
      ]

