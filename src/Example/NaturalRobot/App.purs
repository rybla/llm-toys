module Example.NaturalRobot.App where

import Prelude

import Ai2.Llm (AssistantMsg(..), Msg(..))
import Ai2.Llm as Llm
import Control.Monad.State (get)
import Control.Monad.Writer (tell)
import Data.Argonaut (stringify)
import Data.Lens ((+=), (-=), (.=))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class.Console as Console
import Halogen (liftAff, modify_)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Utility (css, format, prop, todo)

--------------------------------------------------------------------------------

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI component unit =<< HA.awaitBody)

--------------------------------------------------------------------------------

data Direction = Up | Down | Left | Right

type State =
  { robot :: Robot
  , msgs :: Array Msg
  }

data Action = RobotAction RobotAction

data RobotAction
  = MoveNorth Int
  | MoveEast Int
  | MoveSouth Int
  | MoveWest Int

type Robot =
  { x :: Int
  , y :: Int
  }

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: _ -> State
  initialState _ =
    { robot:
        { x: 0
        , y: 0
        }
    , msgs:
        [ Llm.mkUserMsg "Hello my AI assistant!"
        , Llm.mkTextAssistantMsg "Hello my user!"
        ]
    }

  eval = H.mkEval H.defaultEval
    { handleAction = handleAction }

  handleAction (RobotAction ra) = do
    case ra of
      MoveNorth n -> prop @"robot" <<< prop @"y" -= n
      MoveEast n -> prop @"robot" <<< prop @"x" += n
      MoveSouth n -> prop @"robot" <<< prop @"y" += n
      MoveWest n -> prop @"robot" <<< prop @"x" -= n
    { robot } <- get
    Console.log $ "robot: " <> show robot
    Aff.delay (Aff.Milliseconds 500.0) # liftAff

  render state =
    HH.div [ HP.classes [ H.ClassName "App" ] ]
      [ HH.div [ HP.classes [ H.ClassName "Field" ] ]
          [ HH.div
              [ HP.classes [ H.ClassName "Robot" ]
              , css do
                  tell
                    [ "left: {{x}}px" # format { x: show $ state.robot.x * 20 }
                    , "top: {{y}}px" # format { y: show $ state.robot.y * 20 }
                    ]
              ]
              [ HH.div [] [] ]
          ]
      , HH.div [ HP.classes [ H.ClassName "Console" ] ]
          [ HH.div [ HP.classes [ H.ClassName "Transcript" ] ] $
              state.msgs <#> case _ of
                SystemMsg msg -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "System" ] ] [ HH.div [] [ HH.text "System" ], HH.div [] [ HH.text msg.content ] ]
                UserMsg msg -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "User" ] ] [ HH.div [] [ HH.text "User" ], HH.div [] [ HH.text msg.content ] ]
                ToolMsg msg -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Tool" ] ] [ HH.div [] [ HH.text "Tool" ], HH.div [] [ HH.text msg.content ] ]
                AssistantMsg (TextAssistantMsg msg) -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Assistant", H.ClassName "Text" ] ] [ HH.div [] [ HH.text "Assistant" ], HH.div [] [ HH.text $ show msg ] ]
                AssistantMsg (ToolAssistantMsg msg) -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Assistant", H.ClassName "Tool" ] ] [ HH.div [] [ HH.text "Assistant/Tool" ], HH.div [] [ HH.text $ show msg ] ]
                AssistantMsg (StructureAssistantMsg msg) -> HH.div [ HP.classes [ H.ClassName "Msg", H.ClassName "Assistant", H.ClassName "Structure" ] ] [ HH.div [] [ HH.text "Assistant/Structure" ], HH.div [] [ HH.text $ "{ parsed: {{parsed}} }" # format { parsed: stringify msg.parsed } ] ]
          , HH.div [ HP.classes [ H.ClassName "Input" ] ]
              [ HH.button [ HE.onClick $ const $ RobotAction $ MoveWest 1 ] [ HH.text "←" ]
              , HH.button [ HE.onClick $ const $ RobotAction $ MoveNorth 1 ] [ HH.text "↑" ]
              , HH.button [ HE.onClick $ const $ RobotAction $ MoveSouth 1 ] [ HH.text "↓" ]
              , HH.button [ HE.onClick $ const $ RobotAction $ MoveEast 1 ] [ HH.text "→" ]
              ]
          ]
      ]
