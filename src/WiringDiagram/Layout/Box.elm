module WiringDiagram.Layout.Box exposing
    ( Box
    , BoxConfig
    , BoxRelation(..)
    , Polarity(..)
    , Pos
    , Transform
    , boxFromExtent
    , boxify
    , compareBoxes
    , init
    , pad
    , toExtent
    )

import WiringDiagram exposing (..)
import WiringDiagram.Layout.Extent exposing (..)
import WiringDiagram.Vec2 exposing (..)


type alias BoxConfig =
    Box String


type alias Box a =
    { label : Maybe a
    , lo : Vec2
    , width : Float
    , height : Float
    , radius : Float
    , inPorts : Ports
    , outPorts : Ports
    }


setPorts :
    Ports
    -> Ports
    -> { a | inPorts : Ports, outPorts : Ports }
    -> { a | inPorts : Ports, outPorts : Ports }
setPorts i o a =
    { a | inPorts = i, outPorts = o }


type BoxRelation
    = Overlapping
    | LeftOf


type alias Pos =
    Vec2



-- translation only


type alias Transform =
    Vec2


compareBoxes : ( Box a, Box b ) -> BoxRelation
compareBoxes ( a, b ) =
    let
        rightSeparation =
            b.lo.x - (a.lo.x + a.width)
    in
    if rightSeparation < 0 then
        Overlapping

    else
        LeftOf


boxFromExtent : Extent -> Box a
boxFromExtent e =
    { defaultBox
        | lo = e.lo
        , width = e.hi.x - e.lo.x
        , height = e.hi.y - e.lo.y
    }


toExtent : Box a -> Extent
toExtent b =
    { lo = b.lo
    , hi = { x = b.lo.x + b.width, y = b.lo.y + b.height }
    }


type Polarity
    = In
    | Out


pad : Float -> BoxConfig -> BoxConfig
pad v b =
    { b
        | height = b.height + 2 * v
        , width = b.width + 2 * v
    }


boxify : Diagram a -> Box a
boxify (Diagram d) =
    init d.label |> setPorts d.inPorts d.outPorts


defaultBox : Box a
defaultBox =
    init Nothing


init : Maybe a -> Box a
init a =
    { label = a
    , lo = Vec2 0 0
    , width = 64
    , height = 36
    , radius = 1
    , inPorts = []
    , outPorts = []
    }
