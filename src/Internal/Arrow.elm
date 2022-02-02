module Internal.Arrow exposing
    ( Arrow(..), init, translate
    , Meander(..), boundOf, connect, forEdgeWith, isVisible, safe, safeTo, stubForEdge, truncate
    )

{-| Arrow module


## Usage

@docs Arrow, init, translate

-}

import Internal.Bound as Bound exposing (Bound)
import Internal.Extent as Extent exposing (Extent, Polarity(..))
import Internal.Vec2 as Vec2 exposing (Vec2)
import List.Nonempty as NE


{-| Arrow type representing the geometry of an arrow.
-}
type Arrow
    = Arrow
        { tailPoint : Vec2
        , adjustTail : Float
        , meander : Meander
        , adjustHead : Float
        , headPoint : Vec2
        }
    | Port
        { pos : Vec2
        , polarity : Polarity
        , adjust : Float
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


truncate : Polarity -> Arrow -> Arrow
truncate p a =
    case a of
        Arrow arrow ->
            case p of
                In ->
                    Port { pos = arrow.headPoint, polarity = p, adjust = 0 }

                Out ->
                    Port { pos = arrow.tailPoint, polarity = p, adjust = 0 }

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
                , adjustTail = a.adjustTail + b.adjustTail
                , headPoint = { x = b.headPoint.x + dx, y = b.headPoint.y }
                , adjustHead = a.adjustHead + b.adjustHead
                , meander = Direct
                }

        ( Arrow a, Port b ) ->
            Arrow
                { tailPoint = a.tailPoint
                , adjustTail = a.adjustTail + b.adjust
                , headPoint = { x = b.pos.x + dx, y = b.pos.y }
                , adjustHead = a.adjustHead + b.adjust
                , meander = Direct
                }

        ( Port a, Arrow b ) ->
            Arrow
                { tailPoint = a.pos
                , adjustTail = a.adjust + b.adjustTail
                , headPoint = { x = b.headPoint.x + dx, y = b.headPoint.y }
                , meander = Direct
                , adjustHead = a.adjust + b.adjustHead
                }

        ( Port a, Port b ) ->
            Arrow
                { tailPoint = a.pos
                , adjustTail = a.adjust
                , headPoint = { x = b.pos.x + dx, y = b.pos.y }
                , adjustHead = b.adjust
                , meander = Direct
                }


isVisible : Arrow -> Bool
isVisible a =
    case a of
        Arrow arr ->
            arr.tailPoint /= arr.headPoint

        Port _ ->
            False


init : Vec2 -> Vec2 -> Arrow
init from to =
    if from == to then
        Port { pos = from, polarity = In, adjust = 0 }

    else
        Arrow
            { tailPoint = from
            , adjustTail = 0
            , meander = Direct
            , headPoint = to
            , adjustHead = 0
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
stubForEdge : Polarity -> Int -> Int -> Extent -> Arrow
stubForEdge polarity n ofCount e =
    let
        x =
            Extent.side polarity e

        dy =
            max 4 <| Extent.height e / toFloat (2 * ofCount)

        scale =
            toFloat n - (toFloat (ofCount - 1) / 2)
    in
    Port
        { pos =
            { x = x
            , y = dy * scale + Extent.computeCenterY e
            }
        , polarity = polarity
        , adjust = 0
        }


{-| An arrow connecting to a given side of an extent
The Arrow coordinates will be relative to the extent coordinate system
-}
forEdgeWith :
    Config
    -> Polarity
    -> Int
    -> Int
    -> Extent
    -> Arrow
forEdgeWith config side n ofCount e =
    let
        x =
            Extent.side side e

        ( x1, x2 ) =
            case side of
                In ->
                    ( x - config.headLength, x )

                Out ->
                    ( x, x + config.headLength )

        dy =
            max 4 <| Extent.height e / toFloat (2 * ofCount)

        scale =
            toFloat n - (toFloat (ofCount - 1) / 2)
    in
    Arrow
        { tailPoint =
            { x = x1
            , y = dy * scale + Extent.computeCenterY e
            }
        , adjustTail = 0
        , meander = Direct
        , headPoint =
            { x = x2
            , y = dy * scale + Extent.computeCenterY e
            }
        , adjustHead = 0
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


safe : Polarity -> Vec2 -> Arrow -> Arrow
safe _ t a =
    case a of
        Arrow s ->
            Arrow
                { s
                    | adjustTail = t.x + s.adjustTail
                    , adjustHead = t.x + s.adjustHead
                }

        Port p ->
            Port { p | adjust = t.x + p.adjust }


safeTo : Polarity -> Float -> Arrow -> Arrow
safeTo _ x a =
    case a of
        Arrow s ->
            Arrow
                { s
                    | adjustTail = x - s.tailPoint.x
                    , adjustHead = x - s.headPoint.x
                }

        Port p ->
            Port { p | adjust = x - p.pos.x }


translate : Vec2 -> Arrow -> Arrow
translate t a =
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
