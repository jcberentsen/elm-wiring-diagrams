module Cartesian.Layout exposing (toLayout, toLayoutWithConfig)

{-| Layout a cartesian structure


## Usage

@docs toLayout, toLayoutWithConfig

-}

import Diagram.Layout.Config as Config exposing (Config)
import Internal.Cartesian exposing (C)
import Internal.Cartesian.Layout as Layout exposing (Layout)
import Internal.Vec2 exposing (Vec2)


{-| Layout a cartesian structure

This is unfinished, and won't do anything interesting yet.

-}
toLayout : C a -> Layout a
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
toLayoutWithConfig : Config a -> C a -> Layout a
toLayoutWithConfig =
    Layout.layout
