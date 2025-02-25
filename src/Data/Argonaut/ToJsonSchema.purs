module Data.Argonaut.ToJsonSchema where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, JsonDecodeError(..), encodeJson)
import Data.Array as Array
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic, Argument, Constructor(..), NoArguments(..), Product, Sum(..))
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Type.Proxy (Proxy(..))
import Utility (impossible)

class ToJsonSchema (a :: Type) where
  toJsonSchema :: Json

instance ToJsonSchema String where
  toJsonSchema = encodeJson { type: "string" }

instance IsSymbol x => ToJsonSchema (Proxy s) where
  toJsonSchema = encodeJson { type: "string", const: reflectSymbol (Proxy @x) }

instance ToJsonSchema Number where
  toJsonSchema = encodeJson { type: "number" }

instance ToJsonSchema Int where
  toJsonSchema = encodeJson { type: "integer", format: "int46" }

instance ToJsonSchema Boolean where
  toJsonSchema = encodeJson { type: "boolean" }

--------------------------------------------------------------------------------

class LiteralEnum a where
  toLiteralEnum :: a -> String

--------------------------------------------------------------------------------

generic_toJsonSchema :: forall @a rep. Generic a rep => ToJsonSchema_Cons rep => Json
generic_toJsonSchema = toJsonSchema_Cons_final @rep

toJsonSchema_Cons_final :: forall @a. ToJsonSchema_Cons a => Json
toJsonSchema_Cons_final = case toJsonSchema_Cons @a of
  [] -> impossible unit
  [ form ] -> form.value
  forms -> encodeJson { anyOf: forms # map \{ tag, value } -> { tag: mkStringConstSchema tag, value } }

class ToJsonSchema_Cons (a :: Type) where
  toJsonSchema_Cons :: Array { tag :: String, value :: Json }

instance (IsSymbol name, ToJsonSchema_Args a) => ToJsonSchema_Cons (Constructor name a) where
  toJsonSchema_Cons = [ { tag: reflectSymbol (Proxy @name), value: toJsonSchema_Args_final @a } ]

instance (IsSymbol name, ToJsonSchema_Args a, ToJsonSchema_Cons b) => ToJsonSchema_Cons (Sum (Constructor name a) b) where
  toJsonSchema_Cons = { tag: reflectSymbol (Proxy @name), value: toJsonSchema_Args_final @a } Array.: toJsonSchema_Cons @b

toJsonSchema_Args_final :: forall @a. ToJsonSchema_Args a => Maybe Json
toJsonSchema_Args_final = toJsonSchema_Args @a

-- [] -> Nothing
-- [ arg ] -> Just arg

-- args ->
--   Just $ encodeJson { type: "array", minItems: l, maxItems: l, items: args }
--   where
--   l = Array.length args

class ToJsonSchema_Args (a :: Type) where
  toJsonSchema_Args :: Maybe Json

instance ToJsonSchema_Args NoArguments where
  toJsonSchema_Args = Nothing

instance ToJsonSchema a => ToJsonSchema_Args (Argument a) where
  toJsonSchema_Args = Just $ toJsonSchema @a

-- instance (ToJsonSchema a, ToJsonSchema_Args b) => ToJsonSchema_Args (Product (Argument a) b) where
--   toJsonSchema_Args = toJsonSchema @a Array.: toJsonSchema_Args @b

-- --------------------------------------------------------------------------------

-- TODO: update this to respect ToJsonSchema

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

class DecodeJson_Cons a where
  decodeJson_Cons :: { tag :: String, value :: Json } -> JsonDecodeError \/ a

instance (IsSymbol name, DecodeJson_Args a) => DecodeJson_Cons (Constructor name a) where
  decodeJson_Cons { tag, value } | tag == reflectSymbol (Proxy @name) = Constructor <$> decodeJson_Args @a value
  decodeJson_Cons dat = throwError $ UnexpectedValue $ encodeJson dat

instance (IsSymbol name, DecodeJson_Args a, DecodeJson_Cons b) => DecodeJson_Cons (Sum (Constructor name a) b) where
  decodeJson_Cons { tag, value } | tag == reflectSymbol (Proxy @name) = Inl <<< Constructor <$> decodeJson_Args @a value
  decodeJson_Cons dat = Inr <$> decodeJson_Cons @b dat

class DecodeJson_Args a where
  decodeJson_Args :: Json -> JsonDecodeError \/ a

-- instance (Generic a rep, DecodeJsonTaggedSum rep) => DecodeJson (AsTaggedSum a) where
--   decodeJson = decodeJson @{ tag :: String, value :: Json } >=> decodeJsonTaggedSum @rep >=> to >>> AsTaggedSum >>> pure

-- class DecodeJsonTaggedSum a where
--   decodeJsonTaggedSum :: { tag :: String, value :: Json } -> JsonDecodeError \/ a

-- instance (IsSymbol name, DecodeJson a) => DecodeJsonTaggedSum (Constructor name a) where
--   decodeJsonTaggedSum { tag, value } | tag == reflectSymbol (Proxy @name) = Constructor <$> decodeJson value
--   decodeJsonTaggedSum dat = Left $ UnexpectedValue $ encodeJson dat

-- instance (IsSymbol name, DecodeJson a, DecodeJsonTaggedSum b) => DecodeJsonTaggedSum (Sum (Constructor name a) b) where
--   decodeJsonTaggedSum { tag, value } | tag == reflectSymbol (Proxy @name) = Inl <<< Constructor <$> decodeJson value
--   decodeJsonTaggedSum dat = Inr <$> decodeJsonTaggedSum dat

--------------------------------------------------------------------------------

mkStringConstSchema s = encodeJson { type: "string", const: s }

