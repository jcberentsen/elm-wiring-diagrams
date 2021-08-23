module Internal.Svg.Arrow exposing (..)

import Internal.Vec2 exposing (..)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.PathD exposing (Segment(..), pathD)



-- Using https://package.elm-lang.org/packages/Spaxe/svg-pathd/latest/Svg-PathD


arrow :
    { a
        | tailPoint : Vec2
        , adjustTail : Float
        , adjustHead : Float
        , headPoint : Vec2
    }
    -> Svg msg
arrow arr =
    let
        a =
            computeArrowDetails arr
    in
    Svg.path
        [ stroke "grey"
        , strokeOpacity "0.5"
        , fill "transparent"
        , d <|
            pathD
                [ M a.start
                , C a.ascent a.descent a.end
                , M a.headLeft
                , L a.end
                , L a.headRight
                ]
        ]
        []


computeArrowDetails :
    { a
        | headPoint : { b | x : Float, y : Float }
        , tailPoint : { c | x : Float, y : Float }
        , adjustTail : Float
        , adjustHead : Float
    }
    ->
        { start : ( Float, Float )
        , ascent : ( Float, Float )
        , descent : ( Float, Float )
        , end : ( Float, Float )
        , headLeft : ( Float, Float )
        , headRight : ( Float, Float )
        }
computeArrowDetails a =
    let
        gap =
            (a.headPoint.x - a.tailPoint.x - a.adjustTail + a.adjustHead) / 2

        headBack =
            Basics.min 6 (abs (a.tailPoint.x - a.headPoint.x) / 4)

        headSide =
            Basics.min 4 (abs (a.tailPoint.x - a.headPoint.x) / 4)
    in
    { start =
        ( a.tailPoint.x, a.tailPoint.y )
    , ascent =
        ( a.tailPoint.x + a.adjustTail + gap, a.tailPoint.y )
    , descent =
        ( a.headPoint.x + a.adjustHead - gap, a.headPoint.y )
    , end =
        ( a.headPoint.x, a.headPoint.y )
    , headLeft =
        ( a.headPoint.x - headBack, a.headPoint.y - headSide )
    , headRight =
        ( a.headPoint.x - headBack, a.headPoint.y + headSide )
    }
