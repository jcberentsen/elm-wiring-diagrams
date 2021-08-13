module Diagram.Extent exposing (Extent, init)

{-| Represent the horizontal and vertical extents of a two dimensinal area (also called bounding-box).

@docs Extent, init

-}

import Internal.Extent as I
import Internal.Vec2 exposing (Vec2)


{-| Represent the horizontal and vertical extents of a two dimensinal area
-}
type alias Extent =
    I.Extent


{-| Initialize an Extent from two corners, lo and hi
-}
init : Vec2 -> Vec2 -> Extent
init =
    I.init
