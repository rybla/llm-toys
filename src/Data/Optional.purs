module Data.Optional where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Function as Function

foreign import data Optional :: Type -> Type

foreign import undefinedJson :: Json

foreign import undefined_ :: forall a. Optional a
foreign import defined :: forall a. a -> Optional a

foreign import optional :: forall a b. b -> (a -> b) -> Optional a -> b
foreign import optional_ :: forall a b. (Unit -> b) -> (a -> b) -> Optional a -> b

instance Show a => Show (Optional a) where
  show = optional "undefined" show

instance Functor Optional where
  map f = optional undefined_ (f >>> defined)

instance Apply Optional where
  apply = optional (const undefined_) map

instance Applicative Optional where
  pure = defined

instance Bind Optional where
  bind = optional (const undefined_) (flip Function.apply)

instance Monad Optional

instance EncodeJson a => EncodeJson (Optional a) where
  encodeJson = optional undefinedJson encodeJson

instance DecodeJson a => DecodeJson (Optional a) where
  decodeJson json =
    if json == undefinedJson then
      pure undefined_
    else
      defined <$> decodeJson json

