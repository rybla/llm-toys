module Example.MutableWorld.Engine1 where

import Prelude

import Effect (Effect)
import Example.MutableWorld.App (make_main)
import Example.MutableWorld.Common (Engine)

--------------------------------------------------------------------------------

main :: Effect Unit
main = make_main engine

--------------------------------------------------------------------------------

engine :: Engine
engine =
  { name: "Engine1"
  }
