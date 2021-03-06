module Internal.Box exposing
    ( Box
    , BoxConfig
    , BoxRelation(..)
    , Polarity(..)
    , Pos
    , Transform
    , compareBoxes
    , fromExtent
    , init
    , pad
    , toExtent
    )

import Internal.Extent exposing (Extent)
import Internal.Vec2 exposing (..)


type alias BoxConfig =
    Box String


init : Maybe a -> Box a
init a =
    { label = a
    , lo = Vec2 0 0
    , width = 64
    , height = 36
    , radius = 1
    }


type alias Box a =
    { label : Maybe a
    , lo : Vec2
    , width : Float
    , height : Float
    , radius : Float
    }


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


{-| Make a Box from an Extent given an optional label a
-}
fromExtent : Maybe a -> Extent -> Box a
fromExtent a e =
    let
        box =
            init a
    in
    { box
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
