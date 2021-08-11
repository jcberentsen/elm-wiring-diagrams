module Internal.Arrow exposing (..)

{-| Arrow module


## Usage

@docs Arrow, init, translate

-}

import Internal.Extent as Extent exposing (Extent, Polarity(..))
import List.Nonempty as NE
import WiringDiagram.Vec2 as Vec2 exposing (Vec2)


{-| Arrow type representing the geometry of an arrow.
-}
type Arrow
    = Arrow
        { tailPoint : Vec2
        , meander : Meander
        , headPoint : Vec2
        }


type Meander
    = Spline
        { start : Vec2
        , end : Vec2
        }
    | Point Vec2
    | Direct


type alias Config =
    { headLength : Float }


ends : Arrow -> ( Vec2, Vec2 )
ends (Arrow a) =
    ( a.tailPoint, a.headPoint )


{-| Layout a cartesian structure

This is unfinished, and won't do anything interesting yet.

-}
headLength : number
headLength =
    10


init : Vec2 -> Vec2 -> Arrow
init from to =
    Arrow
        { tailPoint = from
        , meander = Direct
        , headPoint = to
        }


extentOf : Arrow -> Extent
extentOf (Arrow a) =
    let
        extents =
            NE.Nonempty { lo = a.tailPoint, hi = a.headPoint } <| meanderExtents a.meander
    in
    Extent.hull extents


{-| An arrow connecting to a given side of an extent
The Arrow coordinates will be relative to the extent coordinate system
-}
forEdge : Polarity -> Int -> Extent -> Arrow
forEdge =
    forEdgeWith { headLength = headLength }


{-| An arrow connecting to a given side of an extent
The Arrow coordinates will be relative to the extent coordinate system
-}
forEdgeWith :
    Config
    -> Polarity
    -> Int
    -> Extent
    -> Arrow
forEdgeWith config side n e =
    let
        x =
            Extent.side side e

        ( x1, x2 ) =
            case side of
                In ->
                    ( x - config.headLength, x )

                Out ->
                    ( x, x + config.headLength )
    in
    Arrow
        { tailPoint =
            { x = x1
            , y = 4 * toFloat n + Extent.computeCenterY e
            }
        , meander = Direct
        , headPoint =
            { x = x2
            , y = 4 * toFloat n + Extent.computeCenterY e
            }
        }


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
