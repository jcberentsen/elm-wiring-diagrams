module Internal.Arrow exposing
    ( Arrow, translate
    , ends, extentOf, headLength, intoLeftEdge, outRightEdge
    )

{-| Arrow module


## Usage

@docs Arrow, init, translate

-}

import Internal.Extent as Extent exposing (Extent)
import List.Nonempty as NE
import WiringDiagram.Vec2 as Vec2 exposing (Vec2)


{-| Layout a cartesian structure

This is unfinished, and won't do anything interesting yet.

-}
headLength : number
headLength =
    10


extentOf : Arrow -> Extent
extentOf (Arrow a) =
    let
        extents =
            NE.Nonempty { lo = a.tailPoint, hi = a.headPoint } <| meanderExtents a.meander
    in
    Extent.hull extents


intoLeftEdge : Extent -> Arrow
intoLeftEdge e =
    Arrow
        { tailPoint = { x = e.lo.x, y = Extent.computeCenterY e }
        , meander = Direct
        , headPoint = { x = e.lo.x + headLength, y = Extent.computeCenterY e }
        }


outRightEdge : Extent -> Arrow
outRightEdge e =
    Arrow
        { tailPoint = { x = e.hi.x, y = Extent.computeCenterY e }
        , meander = Direct
        , headPoint = { x = e.hi.x + headLength, y = Extent.computeCenterY e }
        }


{-| Arrow type representing the geometry of an arrow.
-}
type Arrow
    = Arrow
        { tailPoint : Vec2
        , meander : Meander
        , headPoint : Vec2
        }


ends : Arrow -> ( Vec2, Vec2 )
ends (Arrow a) =
    ( a.tailPoint, a.headPoint )


type Meander
    = Spline
        { start : Vec2
        , end : Vec2
        }
    | Point Vec2
    | Direct


meanderExtents : Meander -> List Extent
meanderExtents m =
    case m of
        Spline s ->
            [ { lo = s.start, hi = s.end } ]

        Point p ->
            [ { lo = p, hi = p } ]

        Direct ->
            []


translate : Vec2 -> Arrow -> Arrow
translate t (Arrow s) =
    let
        tv =
            Vec2.translate t
    in
    Arrow
        { s
            | tailPoint = tv s.tailPoint
            , headPoint = tv s.headPoint
            , meander = translateMeander tv s.meander
        }


translateMeander : (Vec2 -> Vec2) -> Meander -> Meander
translateMeander t m =
    case m of
        Spline s ->
            Spline
                { start = t s.start
                , end = t s.end
                }

        Point p ->
            Point (t p)

        Direct ->
            Direct



--     L.Arrow
--         { label = "arrow"
--         , tail = { x = innerExtent.lo.x, y = computeCenterY innerExtent }
--         , head = { x = innerExtent.lo.x + arrowHeadLength, y = computeCenterY innerExtent }
--         }
-- o =
--     L.Arrow
--         { label = "arrow"
--         , tail =
--             { x = innerExtent.hi.x + arrowHeadLength
--             , y = computeCenterY innerExtent
--             }
--         , head =
--             { x = innerExtent.hi.x + 2 * arrowHeadLength
--             , y = computeCenterY innerExtent
--             }
--         }
