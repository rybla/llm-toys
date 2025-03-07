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
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Halogen.HTML (PlainHTML)
import Halogen.HTML as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Utility (spanC)
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
  , status :: String
  }

type Location =
  { name :: String
  , description :: String
  }

data WorldUpdate
  = CreateCharacter
      { name :: String, description :: String, location_name :: String, status :: String }
  | SetCharacterStatus
      { name :: String, status :: String }
  | SetCharacterLocation
      { name :: String, location_name :: String }
  | CreateLocation
      { name :: String, description :: String }

derive instance Generic WorldUpdate _

instance Show WorldUpdate where
  show x = genericShow x

instance ToJsonSchema WorldUpdate where
  toJsonSchema = generic_toJsonSchema @WorldUpdate

instance EncodeJson WorldUpdate where
  encodeJson x = genericEncodeJson x

instance DecodeJsonFromSchema WorldUpdate where
  decodeJsonFromSchema x = generic_decodeJsonFromSchema x

applyWorldUpdate :: WorldUpdate -> World -> World

applyWorldUpdate (CreateLocation { name, description }) = prop @"locations" <<< at name .~ pure { name, description }

applyWorldUpdate (CreateCharacter { name, description, location_name }) = prop @"characters" <<< at name .~ pure { name, description, location_name, status: "Normal" }
applyWorldUpdate (SetCharacterStatus { name, status }) = prop @"characters" <<< at name %~ map (prop @"status" .~ status)
applyWorldUpdate (SetCharacterLocation { name, location_name }) = prop @"characters" <<< at name %~ map (prop @"location_name" .~ location_name)

describeWorld :: World -> String
describeWorld w = intercalate "\n"
  [ if Map.isEmpty w.locations then
      "    - No locations have been created yet"
    else intercalate "\n" $
      [ "    - Locations:"
      , intercalate "\n" $
          w.locations # Map.toUnfoldable <#> \(_ /\ l) ->
            "        - {{name}}: {{description}}" # format l
      ]
  , ""
  , if Map.isEmpty w.characters then
      "    - No characters have been created yet"
    else intercalate "\n" $
      [ "    - Characters:"
      , intercalate "\n" $
          w.characters # Map.toUnfoldable <#> \(_ /\ c) -> format c $ intercalate "\n"
            [ "        - {{name}}: {{description}}"
            , "            - Current location: {{location_name}}"
            , "            - Current status: {{status}}"
            ]
      ]
  ]

renderWorld :: World -> PlainHTML
renderWorld w =
  HH.ul []
    [ HH.li [] [ HH.b [] [ HH.text "Locations:" ] ]
    , HH.ul [] $ w.locations # Map.toUnfoldable # map \(_ /\ l) ->
        HH.li []
          [ spanC [ H.ClassName "LocationName" ] [] [ HH.text l.name ]
          , HH.text ". "
          , spanC [ H.ClassName "Description" ] [] [ HH.text l.description ]
          ]
    , HH.li [] [ HH.b [] [ HH.text "Characters:" ] ]
    , HH.ul [] $ w.characters # Map.toUnfoldable # map \(_ /\ c) ->
        HH.li []
          [ spanC [ H.ClassName "CharacterName" ] [] [ HH.text c.name ]
          , HH.text ". "
          , spanC [ H.ClassName "Description" ] [] [ HH.text c.description ]
          , HH.ul []
              [ HH.li []
                  [ HH.b [] [ HH.text "Location:" ]
                  , HH.text " "
                  , spanC [ HH.ClassName "LocationName" ] [] [ HH.text $ c.location_name ]
                  ]
              , HH.li []
                  [ HH.b [] [ HH.text "Status:" ]
                  , HH.text " "
                  , spanC [ HH.ClassName "Status" ] [] [ HH.text c.status ]
                  ]
              ]
          ]
    ]

renderWorldUpdate :: WorldUpdate -> PlainHTML
renderWorldUpdate (CreateCharacter wu) =
  renderConstructor "CreateCharacter"
    [ renderNamedArg "name" (spanC [ H.ClassName "CharacterName" ] [] [ HH.text wu.name ])
    , renderNamedArg "description" (spanC [ H.ClassName "Description" ] [] [ HH.text wu.description ])
    , renderNamedArg "status" (spanC [ H.ClassName "Status" ] [] [ HH.text wu.status ])
    ]
renderWorldUpdate (SetCharacterStatus wu) =
  renderConstructor "SetCharacterStatus"
    [ renderNamedArg "name" (spanC [ H.ClassName "CharacterName" ] [] [ HH.text wu.name ])
    , renderNamedArg "status" (spanC [ H.ClassName "Status" ] [] [ HH.text wu.status ])
    ]
renderWorldUpdate (SetCharacterLocation wu) =
  renderConstructor "SetCharacterLocation"
    [ renderNamedArg "name" (spanC [ H.ClassName "CharacterName" ] [] [ HH.text wu.name ])
    , renderNamedArg "location_name" (spanC [ H.ClassName "LocationName" ] [] [ HH.text wu.location_name ])
    ]
renderWorldUpdate (CreateLocation wu) =
  renderConstructor "CreateLocation"
    [ renderNamedArg "name" (spanC [ H.ClassName "LocationName" ] [] [ HH.text wu.name ])
    , renderNamedArg "description" (spanC [ H.ClassName "Description" ] [] [ HH.text wu.description ])
    ]

renderConstructor :: String -> Array PlainHTML -> PlainHTML
renderConstructor name args =
  HH.div [ HP.classes [ H.ClassName "Constructor" ] ]
    [ HH.div [] [ HH.text name ]
    , HH.div [] args
    ]

renderNamedArg :: String -> PlainHTML -> PlainHTML
renderNamedArg name val =
  HH.div [ HP.classes [ H.ClassName "NamedArg" ] ]
    [ HH.div [] [ HH.text (name <> ":") ]
    , HH.div [] [ val ]
    ]
