module UnitList.Decode exposing (unitList)

{-| A simple decoder for UnitList provided for convenience.

# Decoder
@docs unitList

-}

import UnitList exposing (UnitList)
import Json.Decode as Json exposing (Decoder)


{-|
-}
unitList : Decoder a -> Decoder (UnitList a)
unitList =
    Json.map UnitList.singleton
