module Example.DatingSim where

import Prelude

import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Player =
  { name :: String
  , physicality :: String
  , personality :: String
  , charm :: Number
  , empathy :: Number
  , confidence :: Number
  , wittiness :: Number
  , intelligence :: Number
  }

type State =
  { player :: Player
  }

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent { initialState, eval, render }
  where
  initialState :: input -> State
  initialState _ =
    { player:
        { name: "Ash"
        , physicality: "Ash has blue eyes, long black hair, and a thin, athletic build. They may be interested in sports or exercise as well."
        , personality: "In addition to their physical features, Ash is intelligent, kind-hearted, and independent. They enjoy spending time outdoors, traveling, and exploring new things."
        , charm: 0.5
        , empathy: 0.5
        , confidence: 0.5
        , wittiness: 0.5
        , intelligence: 0.5
        }
    }

  eval = H.mkEval H.defaultEval

  render state =
    HH.div
      [ HP.style "display: flex; flex-direction: row; gap: 0.5em;" ]
      [ HH.div
          [ HP.style "flex-grow: 1;" ]
          [ HH.div [] [ HH.text "game" ] ]
      , HH.div
          [ HP.style "flex-grow: 1;" ]
          [ HH.div [] [ HH.text "player" ]
          , HH.div_
              [ HH.table_
                  [ HH.tr_ [ HH.text "name: ", HH.text state.player.name ]
                  , HH.tr_ [ HH.text "personality: ", HH.text state.player.personality ]
                  , HH.tr_ [ HH.text "physicality: ", HH.text state.player.physicality ]
                  , HH.tr_ [ HH.text "charm: ", HH.text $ show state.player.charm ]
                  , HH.tr_ [ HH.text "empathy: ", HH.text $ show state.player.empathy ]
                  , HH.tr_ [ HH.text "confidence: ", HH.text $ show state.player.confidence ]
                  , HH.tr_ [ HH.text "wittiness: ", HH.text $ show state.player.wittiness ]
                  , HH.tr_ [ HH.text "intelligence: ", HH.text $ show state.player.intelligence ]
                  ]
              ]
          ]
      ]

