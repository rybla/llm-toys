module Halogen.Utility where

import Prelude

import Control.Monad.Except (throwError)
import Control.Promise (Promise, toAffE)
import Data.Either (Either)
import Effect (Effect)
import Effect.Aff (Aff)

foreign import copyToClipboard_
  :: { ok :: Either String Unit
     , error :: String -> Either String Unit
     }
  -> String
  -> Effect (Promise (Either String Unit))

copyToClipboard ∷ String → Aff (Either String Unit)
copyToClipboard s = copyToClipboard_ { ok: pure unit, error: throwError } s # toAffE

foreign import readFromClipboard_
  :: { ok :: String -> Either String String
     , error :: String -> Either String String
     }
  -> Effect (Promise (Either String String))

readFromClipboard :: Aff (Either String String)
readFromClipboard = readFromClipboard_ { ok: pure, error: throwError } # toAffE

