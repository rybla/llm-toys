module Example.MutableWorld.World where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.JsonSchema (class DecodeJsonFromSchema, class ToJsonSchema, decodeJsonFromSchema, generic_decodeJsonFromSchema, generic_toJsonSchema)
import Data.Generic.Rep (class Generic)
import Data.Lens ((%~), (.~))
import Data.Lens.At (at)
import Data.Map (Map)
import Utility (prop)

--------------------------------------------------------------------------------

type World =
  { characters :: Map String Character
  , locations :: Map String Location
  }

type Character =
  { name :: String
  , health :: String
  , status :: String
  , location_name :: String
  }

type Location =
  { name :: String
  , description :: String
  }

data WorldUpdate
  = CreateCharacter
      { name :: String, status :: String, location_name :: String }
  | SetCharacterHealth
      { name :: String, health :: String }
  | SetCharacterStatus
      { name :: String, status :: String }
  | SetCharacterLocation
      { name :: String, location_name :: String }
  | CreateLocation
      { name :: String, description :: String }

derive instance Generic WorldUpdate _

instance ToJsonSchema WorldUpdate where
  toJsonSchema = generic_toJsonSchema @WorldUpdate

instance EncodeJson WorldUpdate where
  encodeJson x = genericEncodeJson x

instance DecodeJsonFromSchema WorldUpdate where
  decodeJsonFromSchema x = generic_decodeJsonFromSchema x

applyWorldUpdate :: WorldUpdate -> World -> World

applyWorldUpdate (CreateCharacter { name, location_name }) = prop @"characters" <<< at name .~ pure { name, health: "Healthy", status: "Normal", location_name }
applyWorldUpdate (SetCharacterHealth { name, health }) = prop @"characters" <<< at name %~ map (prop @"health" .~ health)
applyWorldUpdate (SetCharacterStatus { name, status }) = prop @"characters" <<< at name %~ map (prop @"status" .~ status)
applyWorldUpdate (SetCharacterLocation { name, location_name }) = prop @"characters" <<< at name %~ map (prop @"location_name" .~ location_name)

applyWorldUpdate (CreateLocation { name, description }) = prop @"locations" <<< at name .~ pure { name, description }
