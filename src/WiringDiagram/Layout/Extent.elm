module WiringDiagram.Layout.Extent exposing (..)

import WiringDiagram.Vec2 exposing (..)


{-| Represent the horizontal and vertical extents of a two dimensinal area (also called bounding-box).

Extents should be kept in a shared coordinate system

Consider using <https://package.elm-lang.org/packages/r-k-b/elm-interval/latest/> internally

-}
type alias Extent =
    { lo : Vec2
    , hi : Vec2
    }


{-| Represent how two boxes relate to each other when compared
-}
type BoxRelation
    = Overlapping
    | LeftOf


{-| Map a function over the lo and hi corners of the extent

**Note:** This probably exposes a bit too much of the internals, so don't rely on this.
It is possible that the representation will change, perhaps to intervals

-}
map : (b -> a) -> { c | lo : b, hi : b } -> { lo : a, hi : a }
map f e =
    { lo = f e.lo
    , hi = f e.hi
    }


{-| Compare two extents and get their BoxRelation
-}
compare : ( Extent, Extent ) -> BoxRelation
compare ( a, b ) =
    if a.hi.x <= b.lo.x then
        LeftOf

    else
        Overlapping


{-| Find the outer hull of a list of Extents
-}
hull : List Extent -> Extent
hull ls =
    let
        combine b acc =
            let
                lo =
                    { x = min b.lo.x acc.lo.x
                    , y = min b.lo.y acc.lo.y
                    }

                hi =
                    { x = max b.hi.x acc.hi.x
                    , y = max b.hi.y acc.hi.y
                    }
            in
            { acc | lo = lo, hi = hi }
    in
    List.foldr combine empty ls


{-| The empty extent should probably be represented better than this....
This gives an empty extent, but located at the origin.
It would be better located nowhere, as [`hull`](#hull) will be wrong

Perhaps interval algebra is better here?

-}
empty : { lo : { x : number, y : number }, hi : { x : number, y : number } }
empty =
    { lo = { x = 0, y = 0 }
    , hi = { x = 0, y = 0 }
    }


computeCenterY : Extent -> Float
computeCenterY e =
    (e.lo.y + e.hi.y) / 2
