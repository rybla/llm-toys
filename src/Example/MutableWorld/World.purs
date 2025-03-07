module Example.MutableWorld.World where

import Prelude

import Data.Argonaut (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Argonaut.JsonSchema (class DecodeJsonFromSchema, class ToJsonSchema, generic_decodeJsonFromSchema, generic_toJsonSchema)
import Data.Array (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Lens ((%~), (.~))
import Data.Lens.At (at)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Utility (format, prop)

--------------------------------------------------------------------------------

type World =
  { locations :: Map String Location
  , characters :: Map String Character
  }

type Character =
  { name :: String
  , description :: String
  , location_name :: String
  , health :: String
  , status :: String
  }

type Location =
  { name :: String
  , description :: String
  }

data WorldUpdate
  = CreateCharacter
      { name :: String, description :: String, location_name :: String, status :: String }
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

applyWorldUpdate (CreateLocation { name, description }) = prop @"locations" <<< at name .~ pure { name, description }

applyWorldUpdate (CreateCharacter { name, description, location_name }) = prop @"characters" <<< at name .~ pure { name, description, location_name, health: "Healthy", status: "Normal" }
applyWorldUpdate (SetCharacterHealth { name, health }) = prop @"characters" <<< at name %~ map (prop @"health" .~ health)
applyWorldUpdate (SetCharacterStatus { name, status }) = prop @"characters" <<< at name %~ map (prop @"status" .~ status)
applyWorldUpdate (SetCharacterLocation { name, location_name }) = prop @"characters" <<< at name %~ map (prop @"location_name" .~ location_name)

describeWorld :: World -> String
describeWorld w = intercalate "\n"
  [ "Current state of the world:"
  , "  - Locations:"
  , intercalate "\n" $
      w.locations # Map.toUnfoldable <#> \(_ /\ l) ->
        "   - {{name}}: {{description}}" # format l
  , ""
  , "  - Characters:"
  , intercalate "\n" $
      w.characters # Map.toUnfoldable <#> \(_ /\ c) -> format c $ intercalate "\n"
        [ "  - {{name}}: {{description}}"
        , "    - Current location: {{location_name}}"
        , "    - Current health: {{health}}"
        , "    - Current status: {{status}}"
        ]
  ]
