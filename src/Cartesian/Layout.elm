module Cartesian.Layout exposing (toLayout, toLayoutWithConfig)

{-| Layout a cartesian structure


## Usage

@docs toLayout, toLayoutWithConfig

-}

import Internal.Arrow as Arrow exposing (Arrow)
import Internal.Bound as Bound exposing (Bound)
import Internal.Cartesian as C exposing (C(..))
import Internal.Cartesian.Interface as I
import Internal.Cartesian.Layout as Layout
import Internal.Extent as Extent exposing (Extent)
import Internal.WiringDiagram.Layout as L
import List.Nonempty as NE exposing (Nonempty)
import WiringDiagram.Layout.Box as Box
import WiringDiagram.Layout.Config as Config exposing (Config)
import WiringDiagram.Vec2 as Vec2 exposing (Vec2)


{-| Layout a cartesian structure

This is unfinished, and won't do anything interesting yet.

-}
toLayout : C a -> L.Layout a
toLayout =
    let
        defaultConfig =
            Config.init (\_ _ -> "arrow") (Vec2 40 20)
    in
    toLayoutWithConfig <|
        defaultConfig


{-| Layout with some configuration

This is unfinished, and won't do anything interesting yet.

-}
toLayoutWithConfig : Config a -> C a -> L.Layout a
toLayoutWithConfig config c =
    Layout.finalizeLayout <| Layout.layout config c
