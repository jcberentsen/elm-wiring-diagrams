module Cartesian.Layout exposing (toLayout, toLayoutWithConfig)

{-| Layout a cartesian structure


## Usage

@docs toLayout, toLayoutWithConfig

-}

import Diagram.Layout.Config as Config exposing (Config)
import Internal.Cartesian exposing (C)
import Internal.Cartesian.Layout as Layout
import Internal.Cartesian.Layout.Convert as Convert
import Internal.Vec2 exposing (Vec2)
import Internal.WiringDiagram.Layout as L


{-| Layout a cartesian structure

This is unfinished, and won't do anything interesting yet.

-}
toLayout : C a -> L.Layout a
toLayout =
    let
        defaultConfig =
            Config.default |> Config.setSpacing (Vec2 40 20)
    in
    toLayoutWithConfig <|
        defaultConfig


{-| Layout with some configuration

This is unfinished, and won't do anything interesting yet.

-}
toLayoutWithConfig : Config a -> C a -> L.Layout a
toLayoutWithConfig config c =
    Convert.finalizeLayout <| Layout.layout config c
