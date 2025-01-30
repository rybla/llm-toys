module Example.Basic where

import Prelude

import Ai.Llm as Llm
import Data.Either (Either(..))
import Data.Lens ((%=), (.=))
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse_)
import Data.Tuple.Nested ((/\))
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
import Utility (inj, on, prop)
import Web.HTML.HTMLTextAreaElement as HTMLTextAreaElement

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  inputRefLabel = H.RefLabel "input"

  initialState _ =
    { output: ""
    , status: inj @"idle" unit :: Variant (idle :: Unit, working :: Unit)
    }

  eval = H.mkEval H.defaultEval
    { handleAction = case_
        # on @"submit"
            ( \_ -> do
                input_elem <- H.getHTMLElementRef inputRefLabel >>= maybe (throwError (Aff.error "impossible")) pure
                input <- input_elem # HTMLTextAreaElement.fromHTMLElement # maybe (throwError (Aff.error "impossible")) (HTMLTextAreaElement.value >>> liftEffect)
                prop @"status" .= inj @"working" unit
                result <-
                  Llm.generate
                    { apiKey: ""
                    , baseURL: "http://localhost:11434/v1"
                    -- , model: "phi4"
                    , model: "command-r7b"
                    , messages: [ wrap (inj @"user" { name: none, content: input }) ]
                    , tools:
                        [ wrap $ inj @"function"
                            { name: "set_object_color"
                            , description: "Sets the object's color."
                            , parameters: wrap $ Map.fromFoldable [ "color" /\ wrap (inj @"string" { description: "The color to set the object's color to." }) ]
                            }
                        ]
                    , tool_choice: wrap (inj @"required" unit)
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
    }

  render state =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 1.0em;" ]
      [ HH.textarea
          [ HP.ref inputRefLabel
          -- , HP.value $ "What is 2 + 3? Reply with just the numeric result."
          , HP.value $ "Set the object's color to red."
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

