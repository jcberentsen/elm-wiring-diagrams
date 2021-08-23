module Main exposing (..)

import Cartesian as C
import Cartesian.Layout as Layout
import Cartesian.Layout.Svg as Layout
import Cartesian.Svg as Svg
import Diagram.Bound as Bound exposing (Bound)
import Diagram.Layout.Config as Layout
import Diagram.Svg as Diagram
import Diagram.Svg.Config as Svg
import Diagram.Vec2 as Vec2
import Element exposing (Element, alignRight, centerY, column, el, fill, html, padding, paragraph, rgb255, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font


main =
    Element.layout [] <|
        column [ spacing 32 ]
            [ basicCellExample
            , stringLabelsExample
            , seriesExample
            , parallelExample
            , joinExample
            , simpleBypassExample
            , bypassExample
            ]


basicCellExample =
    column
        []
        [ row [ spacing 16 ]
            [ text "A basic cell: "
            , column []
                [ viewDefault basicCell
                , text "View source"
                ]
            ]
        , text "Notice the cell label is just '.' for now, as we haven't specified how to render the label yet (as Strings)"
        , text "Also the cell is a bit small, we'll get back to how to control sizing"
        ]


stringLabelsExample =
    column []
        [ row [ spacing 16 ]
            [ text "A basic cell with string labels: "
            , column []
                [ viewC Layout.default Svg.forStringLabels basicCell
                , text "View source"
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
                , text "View source"
                ]
            ]
        ]


parallelExample =
    column []
        [ row [ spacing 16 ]
            [ text "Cells in parallell: "
            , column []
                [ el [] <| viewC naiveLayoutConfig Svg.forStringLabels axb_cxd
                , text "View source"
                ]
            ]
        ]


joinExample =
    column []
        [ row [ spacing 16 ]
            [ text "Join paths: "
            , column []
                [ el [] <| viewC naiveLayoutConfig Svg.forStringLabels axb_cxd_e
                , text "View source"
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
                , text "View source"
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


bypassExample =
    let
        layoutConfig =
            naiveLayoutConfig
                |> Layout.setLeafExtent (bigBoxFor "b")
    in
    column []
        [ row [ spacing 16 ]
            [ text "Bypass example: "
            , column []
                [ el [] <| viewC layoutConfig Svg.forStringLabels bypass
                , text "View source"
                ]
            ]
        ]



-- advancedSvgConfig =
--     Svg.default
--         |> Svg.withLabelToString labelToString
--         |> Svg.withTextAttributesFunction textAttributes
--         |> Svg.withCellAttributesFunction cellAttributes
--         |> Svg.withCellWrappingFunction cellWrapping
--         |> Svg.withLabelPositionFunction cellTextAlignment


basicCell =
    C.init "Cell"


abc =
    C.init "a"
        |> C.before (C.init "b")
        |> C.before (C.init "c")


axb =
    C.init "a"
        |> C.aside (C.init "b")


bxa =
    C.init "b"
        |> C.aside (C.init "a")


axb_cxd =
    let
        cxd =
            C.init "c"
                |> C.aside (C.init "d")
    in
    axb |> C.before cxd


axb_cxd_e =
    axb_cxd |> C.before (C.initWith 2 1 "e")


bypass =
    let
        source =
            C.initWith 1 3 "src"

        sink =
            C.initWith 2 1 "sink"

        extraLane =
            C.init "bypass"

        conduce =
            axb_cxd_e |> C.aside extraLane
    in
    source |> C.before conduce |> C.before sink


simpleBypass =
    let
        source =
            C.initWith 1 3 "src"

        sink =
            C.initWith 1 1 "sink"
    in
    source |> C.before (sink |> C.aside bxa)


naiveLayoutConfig =
    Layout.default
        |> Layout.setSpacing (Vec2.init 20 16)
        |> Layout.setLeafExtent normalBound


normalBound =
    always
        (Bound.init <|
            { lo = { x = 0, y = 0 }
            , hi = { x = 64, y = 48 }
            }
        )


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
