module Data.TaggedUnion where

import Prelude

import Control.Monad.Except (throwError)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Argonaut as Json
import Data.Either (Either)
import Data.Maybe (Maybe(..), maybe')
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Variant (Variant, case_, inj, on)
import Foreign.Object (Object)
import Foreign.Object as Object
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Proxy (Proxy(..))
import Utility as U

newtype TaggedUnion :: Symbol -> Row Type -> Type
newtype TaggedUnion tag r = TaggedUnion (Variant r)

derive instance Newtype (TaggedUnion tag r) _

make :: forall @tag @k v r_ r. IsSymbol k => Cons k v r_ r => v -> TaggedUnion tag r
make v = TaggedUnion $ inj (Proxy @k) v

instance (IsSymbol tag, RowToList r l, EncodeJson_TaggedUnion tag r l) => EncodeJson (TaggedUnion tag r) where
  encodeJson (TaggedUnion v) = encodeJson_TaggedUnion @tag @r @l v

class EncodeJson_TaggedUnion :: Symbol -> Row Type -> RowList Type -> Constraint
class EncodeJson_TaggedUnion tag r l | l -> r where
  encodeJson_TaggedUnion :: Variant r -> Json

instance EncodeJson_TaggedUnion tag () RL.Nil where
  encodeJson_TaggedUnion = case_

instance
  ( IsSymbol tag
  , IsSymbol k
  , Cons k (Record v) r_ r
  , Lacks tag v
  , EncodeJson (Record v)
  , EncodeJson_TaggedUnion tag r_ l
  ) =>
  EncodeJson_TaggedUnion tag r (RL.Cons k (Record v) l) where
  encodeJson_TaggedUnion = on (Proxy @k)
    ( encodeJson
        >>> Json.toObject
        >>> maybe' (\_ -> unsafeCrashWith "impossible")
          ( Object.insert (reflectSymbol (Proxy @tag)) (reflectSymbol (Proxy @k) # Json.fromString) >>>
              Json.fromObject
          )
    )
    (encodeJson_TaggedUnion @tag @r_ @l)

instance (IsSymbol tag, RowToList r l, DecodeJson_TaggedUnion tag r l) => DecodeJson (TaggedUnion tag r) where
  decodeJson json = do
    o :: Object Json <- decodeJson json
    tag <- case o # Object.lookup (reflectSymbol (Proxy @tag)) of
      Nothing -> throwError $ TypeMismatch $ "tagged union must have a tag property of type string with name: " <> show (reflectSymbol (Proxy @tag))
      Just json_tag -> case json_tag # Json.toString of
        Nothing -> throwError $ TypeMismatch $ "tagged union must have a tag property of type string with name: " <> show (reflectSymbol (Proxy @tag))
        Just tag -> pure tag
    TaggedUnion <$> decodeJson_TaggedUnion @tag @r @l tag o

class DecodeJson_TaggedUnion :: Symbol -> Row Type -> RowList Type -> Constraint
class DecodeJson_TaggedUnion tag r l | l -> r where
  decodeJson_TaggedUnion :: String -> Object Json -> Either JsonDecodeError (Variant r)

instance IsSymbol tag => DecodeJson_TaggedUnion tag () RL.Nil where
  decodeJson_TaggedUnion tag _ = throwError $ TypeMismatch $ "invalid tag: " <> reflectSymbol (Proxy @tag) <> ": " <> tag

instance
  ( IsSymbol tag
  , IsSymbol k
  , Cons k (Record v) r_ r
  , DecodeJson (Record v)
  , DecodeJson_TaggedUnion tag r_ l
  ) =>
  DecodeJson_TaggedUnion tag r (RL.Cons k (Record v) l) where
  decodeJson_TaggedUnion tag o =
    if tag == reflectSymbol (Proxy @k) then
      inj (Proxy @k) <$> decodeJson (o # Json.fromObject)
    else
      decodeJson_TaggedUnion @tag @r_ @l tag o # map (U.expandCons @k)

expandCons :: forall tag @k v r_ r. Cons k v r_ r => TaggedUnion tag r_ -> TaggedUnion tag r
expandCons = unwrap >>> U.expandCons @k >>> wrap

