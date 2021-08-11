module Internal.Extent exposing (..)

import List.Nonempty as NE exposing (Nonempty)
import WiringDiagram.Vec2 as Vec2 exposing (Vec2)


{-| Represent the horizontal and vertical extents of a two dimensinal area (also called bounding-box).

Extents should be kept in a shared coordinate system

Consider using <https://package.elm-lang.org/packages/r-k-b/elm-interval/latest/> internally

Perhaps we should use Maybe instead of special Empty?

-}
type alias Extent =
    { lo : Vec2
    , hi : Vec2
    }


type Polarity
    = In
    | Out


type Rel a
    = Overlap
    | Onside a


type Hand
    = Left


type Altitude
    = Above


type alias ExtentRelation =
    ( Rel Hand, Rel Altitude )


init : Vec2 -> Vec2 -> Extent
init lo hi =
    { lo = lo, hi = hi }


{-| Map a function over the lo and hi corners of the extent

**Note:** This probably exposes a bit too much of the internals, so don't rely on this.
It is possible that the representation will change, perhaps to intervals

-}
translate : Vec2 -> Extent -> Extent
translate t e =
    map (Vec2.translate t) e


map : (b -> a) -> { c | lo : b, hi : b } -> { lo : a, hi : a }
map f e =
    { lo = f e.lo
    , hi = f e.hi
    }


{-| Compare two extents and get their BoxRelation

This implementation is naive and only covers some necessary cases to support testing

-}
compare : ( Extent, Extent ) -> ExtentRelation
compare ( a, b ) =
    let
        attitude =
            if a.hi.x <= b.lo.x then
                Onside Left

            else
                Overlap

        altitude =
            if a.hi.y <= b.lo.y then
                Onside Above

            else
                Overlap
    in
    ( attitude, altitude )


{-| Find the outer hull of a list of Extents
-}
hull : Nonempty Extent -> Extent
hull ls =
    NE.foldl1 combine ls


combine : Extent -> Extent -> Extent
combine l r =
    init
        { x = min l.lo.x r.lo.x
        , y = min l.lo.y r.lo.y
        }
        { x = max l.hi.x r.hi.x
        , y = max l.hi.y r.hi.y
        }


computeCenterY : Extent -> Float
computeCenterY e =
    (e.lo.y + e.hi.y) / 2


height : Extent -> Float
height e =
    e.hi.y - e.lo.y


width : Extent -> Float
width e =
    e.hi.x - e.lo.x


{-| Yield the x position of the side representing the polarity

    -- x for the left side
    side In =
        e.lo.x

    -- x for the right side
    side Out =
        e.hi.x

-}
side : Polarity -> Extent -> Float
side s e =
    case s of
        In ->
            e.lo.x

        Out ->
            e.hi.x
