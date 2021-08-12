module Internal.Arrow exposing
    ( Arrow, translate
    , boundOf, connect, ends, forEdge, forEdgeWith, isVisible, truncate
    )

{-| Arrow module


## Usage

@docs Arrow, init, translate

-}

import Internal.Bound as Bound exposing (Bound)
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
    | Port
        { pos : Vec2
        , polarity : Polarity
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
ends a =
    case a of
        Arrow arr ->
            ( arr.tailPoint, arr.headPoint )

        Port p ->
            ( p.pos, p.pos )


truncate : Polarity -> Arrow -> Arrow
truncate p a =
    case a of
        Arrow arrow ->
            case p of
                In ->
                    Port { pos = arrow.headPoint, polarity = p }

                Out ->
                    Port { pos = arrow.tailPoint, polarity = p }

        Port _ ->
            a


{-| Extent an arrow between two arrows assumed to be adjacent in the x direction
Connect two arrows by taking over the tail and head


# -> + ->

------>

-}
connect : Float -> Arrow -> Arrow -> Arrow
connect dx l r =
    case ( l, r ) of
        ( Arrow a, Arrow b ) ->
            Arrow
                { tailPoint = a.tailPoint
                , headPoint = { x = b.headPoint.x + dx, y = b.headPoint.y }
                , meander = Direct
                }

        ( Arrow a, Port b ) ->
            Arrow
                { tailPoint = a.tailPoint
                , headPoint = { x = b.pos.x + dx, y = b.pos.y }
                , meander = Direct
                }

        ( Port a, Arrow b ) ->
            Arrow
                { tailPoint = a.pos
                , headPoint = { x = b.headPoint.x + dx, y = b.headPoint.y }
                , meander = Direct
                }

        ( Port a, Port b ) ->
            Arrow
                { tailPoint = a.pos
                , headPoint = { x = b.pos.x + dx, y = b.pos.y }
                , meander = Direct
                }


isVisible : Arrow -> Bool
isVisible a =
    case a of
        Arrow arr ->
            arr.tailPoint /= arr.headPoint

        Port _ ->
            False


{-| Layout a cartesian structure

This is unfinished, and won't do anything interesting yet.

-}
headLength : number
headLength =
    10


init : Vec2 -> Vec2 -> Arrow
init from to =
    if from == to then
        Port { pos = from, polarity = In }

    else
        Arrow
            { tailPoint = from
            , meander = Direct
            , headPoint = to
            }


boundOf : Arrow -> Bound
boundOf a =
    case a of
        Arrow arr ->
            Just <| extentOf arr

        _ ->
            Bound.empty


extentOf : { a | tailPoint : Vec2, headPoint : Vec2, meander : Meander } -> Extent
extentOf a =
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
translate t a =
    -- (Arrow s) =
    let
        tv =
            Vec2.translate t
    in
    case a of
        Arrow s ->
            Arrow
                { s
                    | tailPoint = tv s.tailPoint
                    , headPoint = tv s.headPoint
                    , meander = translateMeander tv s.meander
                }

        Port p ->
            Port { p | pos = tv p.pos }


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
