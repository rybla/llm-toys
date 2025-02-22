module Utility where

import Prelude

import Control.Monad.Writer (Writer, execWriter, runWriter)
import Data.Array as Array
import Data.Identity (Identity)
import Data.Lens (Lens')
import Data.Lens.Record as Lens.Record
import Data.List (List(..), (:))
import Data.List as List
import Data.Newtype (unwrap)
import Data.Profunctor (dimap)
import Data.Profunctor.Strong (class Strong)
import Data.Symbol (class IsSymbol)
import Data.Unfoldable (none)
import Data.Variant (Variant)
import Data.Variant as V
import Effect (Effect)
import Halogen.HTML.Properties as HP
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons)
import Type.Prelude (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element)

todo :: forall a. String -> a
todo msg = unsafeCrashWith $ "[[TODO]]\n" <> msg

bug :: forall a. String -> a
bug msg = unsafeCrashWith $ "[[BUG]]\n" <> msg

impossible :: forall a. Unit -> a
impossible _ = bug "impossible"

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

-- iterates from `0` to `n - 1`
forM_count :: forall m a. Monad m => Int -> (Int -> m a) -> m (List a)
forM_count n f = go n none
  where
  go i xs | i == 0 = pure $ List.reverse xs
  go i xs = do
    x <- f (n - i)
    go (i - 1) (List.Cons x xs)

for_count :: forall a. Int -> (Int -> a) -> List a
for_count n f = (forM_count n (f >>> pure) :: Identity _) # unwrap

combinations_count :: { n :: Int, k :: Int } -> Int
combinations_count { n, k } = factorial n / (factorial (n - k) * factorial k)

factorial :: Int -> Int
factorial n | n <= 0 = 1
factorial n = n * factorial (n - 1)

tails :: forall a. List a -> List (List a)
tails xs = go (xs : Nil) xs
  where
  go xss (_ : xs') = go (xss `List.snoc` xs') xs'
  go xss _ = xss

combinations :: forall a. Int -> List a -> List (List a)
combinations 0 _ = List.Nil : List.Nil
combinations n xs = do
  tails xs >>= case _ of
    Nil -> none
    y : ys -> do
      zs <- combinations (n - 1) ys
      pure $ y : zs

foreign import scrollIntoView :: Element -> Effect Unit

css :: forall a w i. Writer (Array String) a -> HP.IProp (style :: String | w) i
css m = HP.style (m # execWriter # Array.intercalate "; ")
