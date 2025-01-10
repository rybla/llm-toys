module Utility where

import Prelude

import Data.Variant (Variant)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons)
import Unsafe.Coerce (unsafeCoerce)

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "TODO: " <> msg

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "BUG: " <> msg

impossible :: forall a. Unit -> a
impossible _ = unsafeCrashWith "impossible"

expandCons :: forall @k @v r_ r. Cons k v r_ r => Variant r_ -> Variant r
expandCons = unsafeCoerce
