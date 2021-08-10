module Cartesian.Layout exposing (toLayout, toLayoutWithConfig)

{-| Layout a cartesian structure


## Usage

@docs toLayout, toLayoutWithConfig

-}

import Internal.Cartesian exposing (C)
import Internal.Cartesian.Layout as Layout
import Internal.WiringDiagram.Layout as L
import WiringDiagram.Layout.Config as Config exposing (Config)
import WiringDiagram.Vec2 exposing (Vec2)


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
