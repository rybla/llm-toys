module Main where

import Prelude

import Data.Argonaut (encodeJson, stringify)
import Data.Argonaut.AsTaggedSum (AsTaggedSum(..))
import Data.Argonaut.Encode (toJsonString)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep as Generic
import Data.TaggedSum (toTaggedSum)
import Effect (Effect)
import Effect.Class.Console as Console

data MyType = MyString String | MyInt Int | MyBoolean Boolean

derive instance Generic MyType _

main :: Effect Unit
main = do
  -- let v = MyString "hello world"
  -- let s = toTaggedSum $ Generic.from v
  -- Console.log $ show s
  -- Console.log $ toJsonString s

  -- let v = MyString "hello world"
  -- Console.log $ toJsonString $ AsTaggedSum v

  pure unit