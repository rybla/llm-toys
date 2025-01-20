module Example.Basic where

import Prelude

import Ai.Llm.Core (generate, noneToolChoice)
import Data.Either (Either(..))
import Data.Lens ((.=))
import Data.Maybe (maybe)
import Data.Optional (defined, optional, undefined_)
import Data.PartialRecord (PartialRecord(..))
import Data.TaggedUnion as TaggedUnion
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
                  generate
                    ( PartialRecord
                        { apiKey: "ollama"
                        , baseURL: "http://localhost:11434/v1" # defined
                        -- , model: "phi4"
                        -- , model: "llama3.2:3b-instruct-q8_0"
                        , model: "command-r7b"
                        , messages:
                            [ TaggedUnion.make @_ @"user" $ PartialRecord
                                { name: undefined_
                                , content:
                                    input
                                }
                            ]
                        , tools: [] # defined
                        , tool_choice: noneToolChoice # defined
                        }
                    ) # liftAff
                case result of
                  Left err -> do
                    prop @"output" .= "error: " <> err
                  Right (PartialRecord msg) -> do
                    prop @"output" .= (msg.content # optional "undefined" identity)
                prop @"status" .= inj @"idle" unit
            )
    }

  render { output, status } =
    HH.div
      [ HP.style "display: flex; flex-direction: column; gap: 1.0em;" ]
      [ HH.textarea
          [ HP.ref inputRefLabel
          , HP.value $ "What is 2 + 3? Reply with just the numeric result."
          ]
      , HH.button
          [ HE.onClick (const (inj @"submit") unit) ]
          [ HH.text "submit" ]
      , HH.div []
          [ status #
              ( case_
                  # on @"idle" (\_ -> HH.text output)
                  # on @"working" (\_ -> HH.text "working...")
              )
          ]
      ]

