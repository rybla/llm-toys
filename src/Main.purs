module Main where

import Prelude

import Data.Lens ((.=))
import Data.Variant (Variant, case_)
import Effect (Effect)
import Effect.Aff (Aff)
import Example.Basic as Basic
import Example.DatingSim as DatingSim
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver as HVD
import Prim.Row (class Cons)
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Utility (inj, on, prop)

main :: Effect Unit
main = HA.runHalogenAff (HVD.runUI appComponent {} =<< HA.awaitBody)

appComponent :: forall query input output. H.Component query input output Aff
appComponent = H.mkComponent { initialState, eval, render }
  where
  initialState _ =
    { -- app: inj @"menu" unit
      app: inj @"DatingSim" unit
    }

  eval = H.mkEval H.defaultEval
    { handleAction = \app -> do
        prop @"app" .= app
    }

  render state = state.app #
    ( case_
        # on @"menu"
            ( \_ ->
                HH.div
                  [ HP.style "display: flex; flex-direction: column; gap: 0.5em;" ]
                  [ menuButton @"Basic"
                  , menuButton @"DatingSim"
                  ]
            )
        # on @"Basic" (\_ -> HH.slot_ (Proxy @"Basic") unit Basic.component unit)
        # on @"DatingSim" (\_ -> HH.slot_ (Proxy @"DatingSim") unit DatingSim.component unit)
    )

menuButton
  :: forall @x xs_ xs output m
   . IsSymbol x
  => Cons x Unit xs_ xs
  => H.ComponentHTML (Variant xs) output m
menuButton = HH.button [ HE.onClick (const (inj @x unit)) ] [ HH.text $ reflectSymbol (Proxy @x) ]

