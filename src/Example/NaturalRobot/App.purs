module Example.NaturalRobot.App where

import Prelude

import Ai2.Llm (AssistantMsg(..), Msg(..))
import Data.Argonaut (stringify)
import Effect (Effect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Utility (format)

--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component unit =<< HA.awaitBody)

--------------------------------------------------------------------------------

data Direction = Up | Down | Left | Right

type State =
  { robot :: Robot
  , msgs :: Array Msg
  }

type Robot =
  { x :: Int
  , y :: Int
  , dir :: Direction
  }

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: _ -> State
  initialState _ =
    { robot:
        { x: 0
        , y: 0
        , dir: Up
        }
    , msgs: []
    }

  eval = H.mkEval H.defaultEval

  render state =
    HH.div [ HP.classes [ H.ClassName "App" ] ]
      [ HH.div [ HP.classes [ H.ClassName "Field" ] ]
          [ HH.div [ HP.classes [ H.ClassName "Robot" ] ] [] ]
      , HH.div [ HP.classes [ H.ClassName "Console" ] ]
          [ HH.div [ HP.classes [ H.ClassName "Transcript" ] ] $
              state.msgs <#> case _ of
                SystemMsg msg -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "System" ] ] [ HH.text msg.content ]
                UserMsg msg -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "User" ] ] [ HH.text msg.content ]
                ToolMsg msg -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Tool" ] ] [ HH.text msg.content ]
                AssistantMsg (TextAssistantMsg msg) -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Assistant", H.ClassName "Text" ] ] [ HH.text $ show msg ]
                AssistantMsg (ToolAssistantMsg msg) -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Assistant", H.ClassName "Tool" ] ] [ HH.text $ show msg ]
                AssistantMsg (StructureAssistantMsg msg) -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Assistant", H.ClassName "Structure" ] ] [ HH.text $ "{ parsed: {{parsed}} }" # format { parsed: stringify msg.parsed } ]
          , HH.div [ HP.classes [ H.ClassName "Input" ] ]
              [ HH.text "{{Input}}" ]
          ]
      ]
