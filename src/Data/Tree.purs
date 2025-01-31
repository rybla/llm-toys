module Data.Tree where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Foldable (class Foldable)
import Data.Generic.Rep (class Generic)
import Data.Ord.Generic (genericCompare)
import Data.Show.Generic (genericShow)
import Data.Traversable (class Traversable)

data Tree a = Tree a (Array a)

derive instance Generic (Tree a) _

instance Show a => Show (Tree a) where
  show x = genericShow x

instance Eq a => Eq (Tree a) where
  eq x = genericEq x

instance Ord a => Ord (Tree a) where
  compare x = genericCompare x

derive instance Functor Tree
derive instance Foldable Tree
derive instance Traversable Tree

