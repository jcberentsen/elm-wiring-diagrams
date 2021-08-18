module Cartesian.Layout.Svg exposing (toSvgWith, viewportFor)

{-| Convert a Layout to Svg


## Usage

@docs toSvgWith, viewportFor

-}

import Diagram.Layout.Config as Layout
import Internal.Cartesian exposing (C)
import Internal.Cartesian.Layout as Layout exposing (Layout)
import Internal.Cartesian.Layout.Svg as Svg
import Internal.Extent as Extent
import Internal.Svg as Svg
import Internal.Svg.Config as Svg
import Internal.Vec2 exposing (Vec2)
import Svg exposing (Svg)


{-| Render a Layout to Svg

The svg styling lets you control fonts, colors, strokes, transparency...

-}
toSvgWith : Svg.Config a msg -> Layout a -> Svg msg
toSvgWith svgConfig layout =
    Svg.layoutToSvgWithConfig svgConfig layout


{-| Compute a viewport large enough for the layout
-}
viewportFor : Layout a -> Svg.Viewport
viewportFor l =
    case Layout.boundOf l of
        Just extent ->
            { width = Extent.width extent + 4
            , height = Extent.height extent + 4
            , xMin = extent.lo.x - 2
            , yMin = extent.lo.y - 2
            }

        _ ->
            Svg.smallViewport
