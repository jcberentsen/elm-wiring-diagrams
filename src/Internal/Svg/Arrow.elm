module Internal.Svg.Arrow exposing (arrow)

import Internal.Vec2 exposing (..)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.PathD exposing (Segment(..), pathD)



-- Using https://package.elm-lang.org/packages/Spaxe/svg-pathd/latest/Svg-PathD


arrow : { a | tail : Vec2, head : Vec2 } -> Svg msg
arrow a =
    let
        start =
            ( a.tail.x, a.tail.y )

        ascent =
            ( (a.tail.x + a.head.x) / 2, a.tail.y )

        descent =
            ( (a.tail.x + a.head.x) / 2, a.head.y )

        end =
            ( a.head.x, a.head.y )

        headBack =
            Basics.min 6 (abs (a.tail.x - a.head.x) / 4)

        headSide =
            Basics.min 4 (abs (a.tail.x - a.head.x) / 4)

        headLeft =
            ( a.head.x - headBack, a.head.y - headSide )

        headRight =
            ( a.head.x - headBack, a.head.y + headSide )
    in
    Svg.path
        [ stroke "grey"
        , strokeOpacity "0.5"
        , fill "transparent"
        , d <|
            pathD
                [ M start
                , C ascent descent end
                , M headLeft
                , L end
                , L headRight
                ]
        ]
        []
