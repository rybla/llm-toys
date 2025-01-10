module Utility where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "TODO: " <> msg

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "BUG: " <> msg

impossible :: forall a. Unit -> a
impossible _ = unsafeCrashWith "impossible"

expandCons :: forall @k @v r_ r. Cons k v r_ r => Variant r_ -> Variant r
expandCons = unsafeCoerce

inj :: forall @x a r_ r. Cons x a r_ r => IsSymbol x => a -> Variant r
inj = V.inj (Proxy @x)

on :: forall @x a b r_ r. Cons x a r_ r => IsSymbol x => (a -> b) -> (Variant r_ -> b) -> Variant r -> b
on = V.on (Proxy @x)
