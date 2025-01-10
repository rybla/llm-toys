module Data.PartialRecord where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, fromObject)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.Optional (Optional, defined, optional, undefined_)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Foreign.Object (Object)
import Foreign.Object as Object
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Prelude (Proxy(..))

newtype PartialRecord r = PartialRecord (Record r)

derive instance Newtype (PartialRecord r) _

instance
  ( RowToList r l
  , EncodeJson_PartialRecord r l
  ) =>
  EncodeJson (PartialRecord r) where
  encodeJson (PartialRecord r) = encodeJson_PartialRecord @_ @l r # fromObject

class EncodeJson_PartialRecord :: Row Type -> RowList Type -> Constraint
class EncodeJson_PartialRecord r l where
  encodeJson_PartialRecord :: Record r -> Object Json

instance EncodeJson_PartialRecord r RL.Nil where
  encodeJson_PartialRecord _ = Object.empty

instance
  ( IsSymbol k
  , EncodeJson v
  , Cons k (Optional v) r_ r
  , EncodeJson_PartialRecord r l
  ) =>
  EncodeJson_PartialRecord r (RL.Cons k (Optional v) l) where
  encodeJson_PartialRecord r =
    ( r # Record.get (Proxy @k) # optional
        identity
        (Object.insert (reflectSymbol (Proxy @k)) <<< encodeJson)
    )
      (encodeJson_PartialRecord @_ @l r)
else instance
  ( IsSymbol k
  , EncodeJson v
  , Cons k v r_ r
  , EncodeJson_PartialRecord r l
  ) =>
  EncodeJson_PartialRecord r (RL.Cons k v l) where
  encodeJson_PartialRecord r =
    Object.insert (reflectSymbol (Proxy @k)) (encodeJson $ Record.get (Proxy @k) r)
      (encodeJson_PartialRecord @_ @l r)

instance (RowToList r l, DecodeJson_PartialRecord r l) => DecodeJson (PartialRecord r) where
  decodeJson json = do
    obj <- decodeJson json
    wrap <$> decodeJson_PartialRecord @r @l obj

class DecodeJson_PartialRecord :: Row Type -> RowList Type -> Constraint
class DecodeJson_PartialRecord r l | l -> r where
  decodeJson_PartialRecord :: Object Json -> Either JsonDecodeError (Record r)

instance DecodeJson_PartialRecord () RL.Nil where
  decodeJson_PartialRecord _ = pure {}

instance
  ( IsSymbol k
  , DecodeJson v
  , Cons k (Optional v) r_ r
  , Lacks k r_
  , DecodeJson_PartialRecord r_ l
  ) =>
  DecodeJson_PartialRecord r (RL.Cons k (Optional v) l) where
  decodeJson_PartialRecord obj = case obj # Object.lookup (reflectSymbol (Proxy @k)) of
    Nothing -> do
      Record.insert (Proxy @k) undefined_ <$> decodeJson_PartialRecord @r_ @l obj
    Just v_json -> do
      v <- decodeJson v_json
      Record.insert (Proxy @k) (defined v) <$> decodeJson_PartialRecord @r_ @l obj
else instance
  ( IsSymbol k
  , DecodeJson v
  , Cons k v r_ r
  , Lacks k r_
  , DecodeJson_PartialRecord r_ l
  ) =>
  DecodeJson_PartialRecord r (RL.Cons k v l) where
  decodeJson_PartialRecord obj = case obj # Object.lookup (reflectSymbol (Proxy @k)) of
    Nothing -> do
      throwError $ TypeMismatch $ "missing field: " <> show (reflectSymbol (Proxy @k))
    Just v_json -> do
      v <- decodeJson v_json
      Record.insert (Proxy @k) v <$> decodeJson_PartialRecord @r_ @l obj
