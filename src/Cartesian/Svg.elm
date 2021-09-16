module Cartesian.Svg exposing (toSvg, toSvgWith, fromDiagram, Styling)

{-| Convert a cartesian structure to Svg


## Usage

@docs toSvg, toSvgWith, fromDiagram, Styling

-}

import Diagram.Layout.Config as Layout
import Internal.Cartesian exposing (C)
import Internal.Cartesian.Layout as Layout
import Internal.Cartesian.Layout.Svg as Svg
import Internal.Svg as Svg
import Internal.Svg.Config as Svg
import Internal.Vec2 exposing (Vec2)
import Svg exposing (Svg)


{-| Layout a cartesian structure and render to Svg

This uses some silly defaults. Use this for early testing.

Use the toSvgWith function to control layout and styling in more detail

-}
toSvg : C a -> Svg msg
toSvg =
    let
        styling =
            { svgConfig = Svg.default
            , layoutConfig = Layout.default |> Layout.setSpacing (Vec2 40 20)
            }
    in
    toSvgWith styling

{-| An alias for toSvg

    Svg.fromDiagram diagram
-}

fromDiagram : C a -> Svg msg
fromDiagram = toSvg


{-| A type for configuring Layout and Svg styling
-}
type alias Styling a msg =
    { layoutConfig : Layout.Config a
    , svgConfig : Svg.Config a msg
    }


{-| Layout and render to Svg with Styling

You supply two configs. One for layout and one for Svg styling.

The layout styling lets you control, sizes, padding, arrow measurements...
The svg styling lets you control fonts, colors, strokes, transparency...

-}
toSvgWith : Styling a msg -> C a -> Svg msg
toSvgWith styling c =
    Svg.layoutToSvgWithConfig styling.svgConfig <| Layout.layout styling.layoutConfig c
