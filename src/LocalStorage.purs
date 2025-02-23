module LocalStorage where

import Prelude

import Data.Maybe (Maybe)
import Data.Unfoldable (none)
import Effect (Effect)

foreign import save :: String -> String -> Effect Unit

foreign import load_ :: { pure :: String -> Maybe String, none :: Maybe String } -> String -> Effect (Maybe String)

load ∷ String → Effect (Maybe String)
load = load_ { pure, none }
