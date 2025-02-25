module Main where

import Prelude

import Data.Argonaut as Argonaut
import Data.Argonaut.ToJsonSchema (class ToJsonSchema, generic_toJsonSchema, toJsonSchema)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Class.Console as Console

data MyType = MyString String | MyInt Int | MyBoolean Boolean

derive instance Generic MyType _

instance ToJsonSchema MyType where
  toJsonSchema = generic_toJsonSchema @MyType

main :: Effect Unit
main = do
  -- let v = MyString "hello world"
  -- let s = toTaggedSum $ Generic.from v
  -- Console.log $ show s
  -- Console.log $ toJsonString s

  -- let v = MyString "hello world"
  -- Console.log $ toJsonString $ AsTaggedSum v

  Console.log $ Argonaut.stringify $ toJsonSchema @MyType