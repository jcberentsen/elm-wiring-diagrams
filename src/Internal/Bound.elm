module Internal.Bound exposing (..)

import Internal.Extent as Extent exposing (Extent)
import List.Nonempty as NE exposing (Nonempty)
import WiringDiagram.Vec2 as Vec2 exposing (Vec2)


type alias Bound =
    Maybe Extent


init : Extent -> Bound
init e =
    Just e


empty : Bound
empty =
    Nothing


extentOf : Bound -> Maybe Extent
extentOf =
    identity


width : Bound -> Float
width =
    Maybe.withDefault 0 << Maybe.map Extent.width


{-| Map a function over the lo and hi corners of the extent

**Note:** This probably exposes a bit too much of the internals, so don't rely on this.
It is possible that the representation will change, perhaps to intervals

-}
translate : Vec2 -> Bound -> Bound
translate t e =
    Maybe.map (Extent.translate t) e


{-| Find the outer hull of a list of Extents
-}
hull : List Bound -> Bound
hull ls =
    case NE.fromList <| List.filterMap identity ls of
        Just ne ->
            Just <| Extent.hull ne

        _ ->
            empty
