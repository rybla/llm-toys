module Data.TaggedSum where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (Constructor(..), Sum(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

newtype TaggedSum (x :: Symbol) a b = TaggedSum (a \/ b)

derive instance Newtype (TaggedSum x a b) _

instance EncodeJson_TaggedSumRight (TaggedSum x a b) => EncodeJson (TaggedSum x a b) where
  encodeJson ts = encodeJson $ encodeJson_TaggedSumRight ts

instance DecodeJson_TaggedSumRight (TaggedSum x a b) => DecodeJson (TaggedSum x a b) where
  decodeJson json = do
    { tag, value } <- decodeJson @{ tag :: String, value :: Json } json
    decodeJson_TaggedSumRight tag value

--------------------------------------------------------------------------------

class EncodeJson_TaggedSumRight a where
  encodeJson_TaggedSumRight :: a -> { tag :: String, value :: Json }

instance (IsSymbol x, EncodeJson a, EncodeJson_TaggedSumRight b) => EncodeJson_TaggedSumRight (TaggedSum x a b) where
  encodeJson_TaggedSumRight (TaggedSum (Left a)) = { tag: reflectSymbol (Proxy @x), value: encodeJson a }
  encodeJson_TaggedSumRight (TaggedSum (Right b)) = encodeJson_TaggedSumRight b

instance EncodeJson_TaggedSumRight Void where
  encodeJson_TaggedSumRight = absurd

--------------------------------------------------------------------------------

class DecodeJson_TaggedSumRight a where
  decodeJson_TaggedSumRight :: String -> Json -> Either JsonDecodeError a

instance (IsSymbol x, DecodeJson a, DecodeJson_TaggedSumRight b) => DecodeJson_TaggedSumRight (TaggedSum x a b) where
  decodeJson_TaggedSumRight tag json | tag == reflectSymbol (Proxy @x) = TaggedSum <<< Left <$> decodeJson @a json
  decodeJson_TaggedSumRight tag json = TaggedSum <<< Right <$> decodeJson_TaggedSumRight tag json

instance DecodeJson_TaggedSumRight Void where
  decodeJson_TaggedSumRight _tag json = Left $ UnexpectedValue json

--------------------------------------------------------------------------------

-- class ToTaggedSum a x t u | a -> x t u where
--   toTaggedSum :: a -> TaggedSum x t u

-- instance ToTaggedSum b => ToTaggedSum (Sum (Constructor x a) b) x a b where
--   toTaggedSum (Inl (Constructor a)) = TaggedSum $ Left a
--   toTaggedSum (Inr b) = ?a