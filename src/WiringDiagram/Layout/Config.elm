module WiringDiagram.Layout.Config exposing
    ( Config
    , init
    , leafExtent
    , setLeafExtent
    , spacing
    )

import Internal.Bound as Bound exposing (Bound)
import WiringDiagram exposing (..)
import WiringDiagram.Vec2 exposing (..)


type Config a
    = Config
        { spacing : Vec2
        , arrowLabler : ArrowLabler a
        , leafExtent : a -> Bound
        }


type alias ArrowLabler a =
    a -> a -> String


spacing : Config a -> Vec2
spacing (Config c) =
    c.spacing


init : ArrowLabler a -> Vec2 -> Config a
init labler s =
    Config
        { spacing = s
        , arrowLabler = labler
        , leafExtent =
            always <|
                Bound.init <|
                    { lo = { x = 0, y = 0 }
                    , hi = { x = 20, y = 20 }
                    }
        }


setLeafExtent : (a -> Bound) -> Config a -> Config a
setLeafExtent v (Config c) =
    Config { c | leafExtent = v }


leafExtent : Config a -> a -> Bound
leafExtent (Config config) v =
    config.leafExtent v
