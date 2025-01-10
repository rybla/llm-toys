module Data.Literal where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Either (Either)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Variant (Variant, case_, inj, on)
import Prim.Row (class Cons)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Prelude (class IsSymbol, Proxy(..), reflectSymbol)
import Utility (expandCons)

newtype Literal xs = Literal (Variant xs)

derive instance Newtype (Literal xs) _

make :: forall @x xs xs_. Cons x Unit xs_ xs => IsSymbol x => Literal xs
make = Literal (inj (Proxy @x) unit)

instance (RowToList r l, EncodeJson_Literal r l) => EncodeJson (Literal r) where
  encodeJson = unwrap >>> encodeJson_Literal @r @l

class EncodeJson_Literal :: Row Type -> RowList Type -> Constraint
class EncodeJson_Literal r l | l -> r where
  encodeJson_Literal :: Variant r -> Json

instance EncodeJson_Literal () RL.Nil where
  encodeJson_Literal = case_

instance
  ( IsSymbol k
  , Cons k v r_ r
  , EncodeJson_Literal r_ l
  ) =>
  EncodeJson_Literal r (RL.Cons k v l) where
  encodeJson_Literal =
    on
      (Proxy @k)
      (const (encodeJson (reflectSymbol (Proxy @k))))
      (encodeJson_Literal @r_ @l)

instance (RowToList r l, DecodeJson_Literal r l) => DecodeJson (Literal r) where
  decodeJson json = do
    str <- decodeJson @String json
    wrap <$> decodeJson_Literal @r @l str

class DecodeJson_Literal :: Row Type -> RowList Type -> Constraint
class DecodeJson_Literal r l | l -> r where
  decodeJson_Literal :: String -> Either JsonDecodeError (Variant r)

instance DecodeJson_Literal () RL.Nil where
  decodeJson_Literal str = throwError $ TypeMismatch $ "invalid literal: " <> show str

instance
  ( IsSymbol k
  , Cons k Unit r_ r
  , DecodeJson_Literal r_ l
  ) =>
  DecodeJson_Literal r (RL.Cons k v l) where
  decodeJson_Literal str =
    if str == reflectSymbol (Proxy @k) then
      pure $ inj (Proxy @k) unit
    else
      expandCons @k <$> decodeJson_Literal @r_ @l str
