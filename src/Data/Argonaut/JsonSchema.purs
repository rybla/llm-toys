module Data.Argonaut.JsonSchema where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Either.Nested (type (\/))
import Data.Generic.Rep (class Generic, Argument(..), Constructor(..), NoArguments(..), Sum(..), to)
import Data.Maybe (Maybe(..), maybe)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Utility (impossible)

class ToJsonSchema (a :: Type) where
  toJsonSchema :: Json

instance ToJsonSchema String where
  toJsonSchema = encodeJson { type: "string" }

instance IsSymbol x => ToJsonSchema (Proxy s) where
  toJsonSchema = encodeJson { type: "string", const: reflectSymbolAsTag @x }

instance ToJsonSchema Number where
  toJsonSchema = encodeJson { type: "number" }

instance ToJsonSchema Int where
  toJsonSchema = encodeJson { type: "integer", format: "int46" }

instance ToJsonSchema Boolean where
  toJsonSchema = encodeJson { type: "boolean" }

instance ToJsonSchema a => ToJsonSchema (Array a) where
  toJsonSchema = encodeJson { type: "array", items: toJsonSchema @a }

instance (RowToList r l, ToJsonSchema_RowList r l) => ToJsonSchema (Record r) where
  toJsonSchema = encodeJson
    { type: "object"
    , properties: o
    , required: Object.keys o
    , additionalProperties: false
    }
    where
    o = toJsonSchema_RowList @r @l

class ToJsonSchema_RowList (r :: Row Type) (l :: RowList Type) where
  toJsonSchema_RowList :: Object Json

instance ToJsonSchema_RowList r RL.Nil where
  toJsonSchema_RowList = Object.empty

instance
  ( IsSymbol x
  , Cons x a r_ r
  , ToJsonSchema a
  , ToJsonSchema_RowList r l
  ) =>
  ToJsonSchema_RowList r (RL.Cons x a l) where
  toJsonSchema_RowList = Object.insert (reflectSymbol (Proxy @x)) (toJsonSchema @a) $ toJsonSchema_RowList @r @l

--------------------------------------------------------------------------------

generic_toJsonSchema :: forall @a rep. Generic a rep => ToJsonSchema_Cons rep => Json
generic_toJsonSchema = toJsonSchema_Cons_final @rep

toJsonSchema_Cons_final :: forall @a. ToJsonSchema_Cons a => Json
toJsonSchema_Cons_final = case toJsonSchema_Cons @a of
  [] -> impossible unit
  [ con ] -> f con
  cons -> encodeJson { anyOf: cons # map f }
  where
  f con = con.value # maybe (mkStringSchema con.type) \value -> encodeJson { type: mkStringSchema con.type, value }

class ToJsonSchema_Cons (a :: Type) where
  toJsonSchema_Cons :: Array { type :: String, value :: Maybe Json }

toTaggedConstructor :: forall @name. IsSymbol name => Maybe Json -> Array { type :: String, value :: Maybe Json }
toTaggedConstructor Nothing = [ { type: reflectSymbolAsTag @name, value: Nothing } ]
toTaggedConstructor (Just arg) = [ { type: reflectSymbolAsTag @name, value: Just arg } ]

instance (IsSymbol name, ToJsonSchema_Args a) => ToJsonSchema_Cons (Constructor name a) where
  toJsonSchema_Cons = toTaggedConstructor @name (toJsonSchema_Args @a)

instance (ToJsonSchema_Cons a, ToJsonSchema_Cons b) => ToJsonSchema_Cons (Sum a b) where
  toJsonSchema_Cons = toJsonSchema_Cons @a <> toJsonSchema_Cons @b

-- only instances are for 0 arguments or 1 argument
class ToJsonSchema_Args (a :: Type) where
  toJsonSchema_Args :: Maybe Json

instance ToJsonSchema_Args NoArguments where
  toJsonSchema_Args = Nothing

instance ToJsonSchema a => ToJsonSchema_Args (Argument a) where
  toJsonSchema_Args = Just $ toJsonSchema @a

--------------------------------------------------------------------------------

class DecodeJsonFromSchema a where
  decodeJsonFromSchema :: Json -> JsonDecodeError \/ a

instance DecodeJsonFromSchema String where
  decodeJsonFromSchema json = decodeJson json

instance DecodeJsonFromSchema Number where
  decodeJsonFromSchema json = decodeJson json

instance DecodeJsonFromSchema Int where
  decodeJsonFromSchema json = decodeJson json

instance DecodeJsonFromSchema Boolean where
  decodeJsonFromSchema json = decodeJson json

instance DecodeJsonFromSchema a => DecodeJsonFromSchema (Array a) where
  decodeJsonFromSchema = decodeJson @(Array Json) >=> traverse decodeJsonFromSchema

instance (RowToList r l, DecodeJsonFromSchema_RowList r l) => DecodeJsonFromSchema (Record r) where
  decodeJsonFromSchema = decodeJson @(Object Json) >=> decodeJsonFromSchema_Row @r @l

class DecodeJsonFromSchema_RowList r (l :: RowList Type) | l -> r where
  decodeJsonFromSchema_Row :: Object Json -> JsonDecodeError \/ Record r

instance DecodeJsonFromSchema_RowList () RL.Nil where
  decodeJsonFromSchema_Row _ = pure {}

instance
  ( IsSymbol x
  , DecodeJsonFromSchema a
  , Cons x a r r'
  , Lacks x r
  , DecodeJsonFromSchema_RowList r l
  ) =>
  DecodeJsonFromSchema_RowList r' (RL.Cons x a l) where
  decodeJsonFromSchema_Row o = case Object.lookup x o of
    Nothing -> throwError $ AtKey x MissingValue
    Just json_a -> lmap (AtKey x) do
      Record.insert (Proxy @x)
        <$> decodeJsonFromSchema @a json_a
        <*> decodeJsonFromSchema_Row @r @l o
    where
    x = reflectSymbol (Proxy @x)

generic_decodeJsonFromSchema :: forall @a rep. Generic a rep => DecodeJsonFromSchema_Cons rep => Json -> JsonDecodeError \/ a
generic_decodeJsonFromSchema = map to <<< decodeJsonFromSchema_Cons_final @rep

decodeJsonFromSchema_Cons_final :: forall @a. DecodeJsonFromSchema_Cons a => Json -> JsonDecodeError \/ a
decodeJsonFromSchema_Cons_final json = do
  let
    decodings :: Array (JsonDecodeError \/ a)
    decodings = case decodeJson @{ type :: String, value :: Json } json of
      Right result -> decodeJsonFromSchema_Cons $ Tagged result
      Left _ -> case decodeJson @String json of
        Right result -> decodeJsonFromSchema_Cons $ String result
        Left _ -> decodeJsonFromSchema_Cons $ Untagged json
  case Array.head decodings of
    Nothing -> throwError $ UnexpectedValue json
    Just a -> a

data DecodeJsonFromSchema_Cons_Input
  = Tagged { type :: String, value :: Json }
  | String String
  | Untagged Json

class DecodeJsonFromSchema_Cons a where
  decodeJsonFromSchema_Cons :: DecodeJsonFromSchema_Cons_Input -> Array (JsonDecodeError \/ a)

instance (DecodeJsonFromSchema_Cons a, DecodeJsonFromSchema_Cons b) => DecodeJsonFromSchema_Cons (Sum a b) where
  decodeJsonFromSchema_Cons type_value = (decodeJsonFromSchema_Cons @a type_value # map (map Inl)) <> (decodeJsonFromSchema_Cons @b type_value # map (map Inr))

-- this handles the 0 arguments case, so doesn't have to be handled by DecodeJsonFromSchema_Args
instance IsSymbol name => DecodeJsonFromSchema_Cons (Constructor name NoArguments) where
  decodeJsonFromSchema_Cons (String s) | reflectSymbolAsTag @name == s = [ Right $ Constructor NoArguments ]
  decodeJsonFromSchema_Cons _ = []
else instance (IsSymbol name, DecodeJsonFromSchema_Args a) => DecodeJsonFromSchema_Cons (Constructor name a) where
  decodeJsonFromSchema_Cons (Tagged { type: t, value }) | t == reflectSymbolAsTag @name = [ Constructor <$> decodeJsonFromSchema_Args @a value ]
  decodeJsonFromSchema_Cons (String s) = [ Constructor <$> decodeJsonFromSchema_Args @a (encodeJson s) ]
  decodeJsonFromSchema_Cons (Untagged json) = [ Constructor <$> decodeJsonFromSchema_Args @a json ]
  decodeJsonFromSchema_Cons _ = []

-- only instances are for 1 argument
class DecodeJsonFromSchema_Args a where
  decodeJsonFromSchema_Args :: Json -> JsonDecodeError \/ a

instance DecodeJsonFromSchema a => DecodeJsonFromSchema_Args (Argument a) where
  decodeJsonFromSchema_Args = map Argument <<< decodeJsonFromSchema

--------------------------------------------------------------------------------

reflectSymbolAsTag :: forall @x. IsSymbol x => String
reflectSymbolAsTag = reflectSymbol (Proxy @x)

mkStringSchema ∷ String → Json
mkStringSchema s = encodeJson { type: "string", const: s }

