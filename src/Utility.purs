module Utility where

import Prelude

import Data.Lens (Lens')
import Data.Lens.Record as Lens.Record
import Data.Profunctor (dimap)
import Data.Profunctor.Strong (class Strong)
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

expandCons :: forall @k @v @r_ r. Cons k v r_ r => Variant r_ -> Variant r
expandCons = unsafeCoerce

inj :: forall @x a r_ r. Cons x a r_ r => IsSymbol x => a -> Variant r
inj = V.inj (Proxy @x)

on :: forall @x a b r_ r. Cons x a r_ r => IsSymbol x => (a -> b) -> (Variant r_ -> b) -> Variant r -> b
on = V.on (Proxy @x)

onLens :: forall @x a r_ r. Cons x a r_ r => IsSymbol x => (Variant r_ -> a) -> Lens' (Variant r) a
onLens other = dimap (on @x identity other) (inj @x)

onLens' :: forall @x a r_ r. Cons x a r_ r => IsSymbol x => Lens' (Variant r) a
onLens' = onLens @x \_ -> impossible unit

prop :: forall @l r1 r2 r a b. IsSymbol l => Cons l a r r1 => Cons l b r r2 => (forall p. Strong p => p a b -> p (Record r1) (Record r2))
prop = Lens.Record.prop (Proxy @l)

for_count :: forall m. Monad m => Int -> m Unit -> m Unit
for_count i _ | i <= 0 = pure unit
for_count i m = do
  m
  for_count (i - 1) m

