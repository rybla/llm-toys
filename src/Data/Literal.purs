module Data.Literal where

import Prelude

import Prim.Row (class Cons)
import Type.Prelude (class IsSymbol, Proxy)

newtype Literal xs = Literal (forall r. LiteralK xs r -> r)
type LiteralK xs r = forall x xs_. IsSymbol x => Cons x Unit xs_ xs => Proxy x -> r

mkLiteral :: forall xs. LiteralK xs (Literal xs)
mkLiteral a = Literal \k -> k a

runLiteral :: forall xs r. LiteralK xs r -> Literal xs -> r
runLiteral k1 (Literal k2) = k2 k1

foreign import data LiteralOr :: Symbol -> Type -> Type

