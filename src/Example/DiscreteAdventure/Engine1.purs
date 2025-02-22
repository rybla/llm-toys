module Example.DiscreteAdventure.Engine1 where

import Prelude

import Effect (Effect)
import Example.DiscreteAdventure (Engine, main_component)
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver as HVD

type World =
  { player_name :: String
  }

engine :: Engine World
engine =
  { initial_world:
      { player_name: "Glorbax the Unready" }
  , renderWorld: \world ->
      HH.div
        []
        [ HH.text $ show world ]
  , initial_transcript:
      []
  , initial_choices:
      [ { full_description: "Begin the story."
        , short_description: "Begin the story."
        , update:
            { description: "Identity"
            , apply: identity
            }
        }
      ]
  }

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI main_component { engine } =<< HA.awaitBody)

