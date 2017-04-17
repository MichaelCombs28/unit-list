module UnitList.Decode exposing (unitList)

import UnitList exposing (UnitList, singleton)
import Json.Decode as Json exposing (Decoder)


unitList : Decoder a -> Decoder (UnitList a)
unitList =
    Json.map singleton
