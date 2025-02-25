module Data.Argonaut.AsTaggedSum where

import Prelude

import Data.Argonaut (class DecodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Argonaut.ToJsonSchema (class ToJsonSchema, toJsonSchema)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic, Argument, Constructor(..), Product, Sum(..), to)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))

newtype AsTaggedSum a = AsTaggedSum a

derive instance Newtype (AsTaggedSum a) _

--------------------------------------------------------------------------------

generic_toJsonSchema :: forall @a rep. Generic a rep => ToJsonSchema_Cons rep => Json
generic_toJsonSchema = case toJsonSchema_Cons @rep of
  [ form ] -> form.value
  forms -> encodeJson { anyOf: forms }

class ToJsonSchema_Cons (a :: Type) where
  toJsonSchema_Cons :: Array { tag :: String, value :: Json }

instance (IsSymbol name, ToJsonSchema_Args a) => ToJsonSchema_Cons (Constructor name a) where
  toJsonSchema_Cons = [ { tag: reflectSymbol (Proxy @name), value: toJsonSchema_Args @a } ]

instance (IsSymbol name, ToJsonSchema_Args a, ToJsonSchema_Cons b) => ToJsonSchema_Cons (Sum (Constructor name a) b) where
  toJsonSchema_Cons = { tag: reflectSymbol (Proxy @name), value: toJsonSchema_Args @a } Array.: toJsonSchema_Cons @b

class ToJsonSchema_Args (a :: Type) where
  toJsonSchema_Args :: Json

instance ToJsonSchema_Args' a => ToJsonSchema_Args a where
  toJsonSchema_Args = case toJsonSchema_Args' @a of
    [ arg ] -> arg
    args ->
      encodeJson { type: "array", minItems: l, maxItems: l, items: args }
      where
      l = Array.length args

class ToJsonSchema_Args' (a :: Type) where
  toJsonSchema_Args' :: Array Json

instance ToJsonSchema a => ToJsonSchema_Args' (Argument a) where
  toJsonSchema_Args' = [ toJsonSchema @a ]

instance (ToJsonSchema a, ToJsonSchema_Args' b) => ToJsonSchema_Args' (Product (Argument a) b) where
  toJsonSchema_Args' = toJsonSchema @a Array.: toJsonSchema_Args' @b

-- --------------------------------------------------------------------------------

-- instance (Generic a rep, EncodeJson_Sum rep) => EncodeJson (AsTaggedSum a) where
--   encodeJson = unwrap >>> from >>> encodeJson_Sum >>> encodeJson

-- class EncodeJson_Sum a where
--   encodeJson_Sum :: a -> { tag :: String, value :: Json }

-- instance (IsSymbol name, EncodeJson_Args a) => EncodeJson_Sum (Constructor name a) where
--   encodeJson_Sum (Constructor a) = { tag: reflectSymbol (Proxy @name), value: encodeJson_Args a }

-- instance (IsSymbol name, EncodeJson_Args a, EncodeJson_Sum b) => EncodeJson_Sum (Sum (Constructor name a) b) where
--   encodeJson_Sum (Inl (Constructor a)) = { tag: reflectSymbol (Proxy @name), value: encodeJson_Args a }
--   encodeJson_Sum (Inr b) = encodeJson_Sum b

-- class EncodeJson_Args a where
--   encodeJson_Args :: a -> Json

-- instance EncodeJson_Args' a => EncodeJson_Args a where
--   encodeJson_Args a =
--     let
--       args = encodeJson_Args' a
--       l = Array.length args
--     in
--       encodeJson { type: "array", minItems: l, maxItems: l, items: args }

-- class EncodeJson_Args' a where
--   encodeJson_Args' :: a -> Array Json

-- instance EncodeJson a => EncodeJson_Args' NoArguments where
--   encodeJson_Args' NoArguments = []

-- instance EncodeJson a => EncodeJson_Args' (Argument a) where
--   encodeJson_Args' (Argument a) = [ encodeJson a ]

-- instance (EncodeJson a, EncodeJson_Args' b) => EncodeJson_Args' (Product (Argument a) b) where
--   encodeJson_Args' (Product (Argument a) b) = encodeJson a Array.: encodeJson_Args' b

--------------------------------------------------------------------------------

-- TODO: fix this to respect ToJsonSchema

instance (Generic a rep, DecodeJsonTaggedSum rep) => DecodeJson (AsTaggedSum a) where
  decodeJson = decodeJson @{ tag :: String, value :: Json } >=> decodeJsonTaggedSum @rep >=> to >>> AsTaggedSum >>> pure

class DecodeJsonTaggedSum a where
  decodeJsonTaggedSum :: { tag :: String, value :: Json } -> JsonDecodeError \/ a

instance (IsSymbol name, DecodeJson a) => DecodeJsonTaggedSum (Constructor name a) where
  decodeJsonTaggedSum { tag, value } | tag == reflectSymbol (Proxy @name) = Constructor <$> decodeJson value
  decodeJsonTaggedSum dat = Left $ UnexpectedValue $ encodeJson dat

instance (IsSymbol name, DecodeJson a, DecodeJsonTaggedSum b) => DecodeJsonTaggedSum (Sum (Constructor name a) b) where
  decodeJsonTaggedSum { tag, value } | tag == reflectSymbol (Proxy @name) = Inl <<< Constructor <$> decodeJson value
  decodeJsonTaggedSum dat = Inr <$> decodeJsonTaggedSum dat

