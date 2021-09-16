module Main exposing (..)

import Cartesian as C
import Cartesian.Examples exposing (..)
import Cartesian.Layout as Layout
import Cartesian.Layout.Svg as Layout
import Cartesian.Svg as Svg
import Diagram.Bound as Bound exposing (Bound)
import Diagram.Layout.Config as Layout
import Diagram.Svg as Diagram
import Diagram.Svg.Config as Svg
import Diagram.Vec2 as Vec2
import Element exposing (Element, alignRight, centerX, centerY, column, el, fill, html, padding, paragraph, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Region as Region


main =
    Element.layout [ padding 16] <|
        column [ spacing 48]
            [ viewLogo
            , motivatingExample
            , basicCellExample
            , stringLabelsExample
            , seriesExample
            , parallelExample
            , joinExample
            , simpleBypassExample
            , bypassExample
            ]


viewLogo =
    el [ centerX ] <| viewInStyle logo


motivatingExample =
    column [ spacing 16 ]
        [ column [ spacing 16 ]
            [ h 1 "Tutorial on creating Cartesian Diagrams"
            , h 2 "This tutorial will show you how to get from a simple diagram: "
            , viewDefault abc
            , h 2 "to a larger stylish(tbd) interactive(tbd) diagram like this:"
            , viewInStyle showOff
            ]
        ]


showOff =
    bypass


h n =
    el [ Region.heading n, Font.size (max 8 (48 - n * 6)) ] << text


basicCellExample =
    column [ padding 16, spacing 16 ]
        [ column [ spacing 16 ]
            [ h 1 "Basics: "
            , text "Let's start with the simplest possible part, a cell. Think box"
            , viewDefault basicCell
            ]
        , text "Notice the cell label is just '.' for now, as we haven't specified how to render the label yet (as Strings)"
        , text "Also the cell is a bit small, we'll get back to how to control sizing"
        ]


stringLabelsExample =
    column []
        [ row [ spacing 16 ]
            [ text "So let's render the cell label String: "
            , column []
                [ viewC Layout.default Svg.forStringLabels basicCell
                ]
            ]
        , paragraph []
            [ text "Use forStringLabels when labels are just String. "
            , text "We may want to have the cell payload be a larger type, then we must supply a function to convert it to a String. "
            , text "You may notice that the rendered text does'nt quite fit the box. We can deal with this by controlling the box extent or the font (or both). "
            ]
        ]


seriesExample =
    column []
        [ row [ spacing 16 ]
            [ text "Cells in series: "
            , column []
                [ el [] <| viewC naiveLayoutConfig Svg.forStringLabels abc
                ]
            ]
        ]


parallelExample =
    column []
        [ row [ spacing 16 ]
            [ text "Cells in parallell: "
            , column []
                [ el [] <| viewC naiveLayoutConfig Svg.forStringLabels axb_cxd
                ]
            ]
        ]


joinExample =
    column []
        [ row [ spacing 16 ]
            [ text "Join paths: "
            , column []
                [ el [] <| viewC naiveLayoutConfig Svg.forStringLabels axb_cxd_e
                ]
            ]
        , paragraph []
            [ text "We use initWith to specify 2 incoming ports on the 'e' cell. "
            , text "If we left the arity of 'e' as (1,1) it would only connect from the 'c'. "
            , text "Leaving the 'd' node to have a free connection that can be connected to something else next. "
            ]
        ]


simpleBypassExample =
    let
        layoutConfig =
            naiveLayoutConfig
                |> Layout.setLeafExtent (bigBoxFor "b")
    in
    column []
        [ row [ spacing 16 ]
            [ text "Bypass example: "
            , column []
                [ el [] <| viewC layoutConfig Svg.forStringLabels simpleBypass
                ]
            ]
        ]


bigBoxFor str label =
    if label == str then
        Bound.init <|
            { lo = { x = 0, y = 0 }
            , hi = { x = 400, y = 150 }
            }

    else
        normalBound label


viewInStyle diagram =
    let
        layoutConfig =
            naiveLayoutConfig
                |> Layout.setLeafExtent (bigBoxFor "b")
    in
    viewC layoutConfig Svg.forStringLabels diagram


bypassExample =
    column []
        [ row [ spacing 16 ]
            [ text "Bypass example: "
            , viewInStyle bypass
            ]
        ]



-- advancedSvgConfig =
--     Svg.default
--         |> Svg.withLabelToString labelToString
--         |> Svg.withTextAttributesFunction textAttributes
--         |> Svg.withCellAttributesFunction cellAttributes
--         |> Svg.withCellWrappingFunction cellWrapping
--         |> Svg.withLabelPositionFunction cellTextAlignment


naiveLayoutConfig =
    Layout.default
        |> Layout.setSpacing (Vec2.init 24 16)
        |> Layout.setLeafExtent normalBound


normalBound l =
    Bound.init <|
        { lo = { x = 0, y = 0 }
        , hi =
            { x = max 64 (16 * toFloat (String.length l))
            , y = 48
            }
        }


viewDefault diagram =
    viewC Layout.default Svg.default diagram


viewC layoutConfig svgConfig diagram =
    let
        layout =
            Layout.toLayoutWithConfig layoutConfig diagram

        viewport =
            Layout.viewportFor layout
    in
    el [] <|
        html <|
            Diagram.view viewport
                [ Layout.toSvgWith svgConfig layout
                ]
