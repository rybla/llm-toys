module Example.DiscreteAdventure.Engine1.App where

import Prelude

import Effect (Effect)
import Example.DiscreteAdventure (main_component)
import Example.DiscreteAdventure.Engine1.Common (engine)
import Halogen.Aff as HA
import Halogen.VDom.Driver as HVD

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI main_component { engine } =<< HA.awaitBody)
