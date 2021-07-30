module WiringDiagram.Layout.Config exposing
    ( Config
    , init
    , spacing
    )

import WiringDiagram exposing (..)
import WiringDiagram.Vec2 as Vec exposing (..)


type Config a
    = Config
        { spacing : Vec2
        , arrowLabler : ArrowLabler a
        }


type alias ArrowLabler a =
    a -> a -> String


spacing : Config a -> Vec2
spacing (Config c) =
    c.spacing


init : ArrowLabler a -> Vec2 -> Config a
init labler s =
    Config { spacing = s, arrowLabler = labler }
