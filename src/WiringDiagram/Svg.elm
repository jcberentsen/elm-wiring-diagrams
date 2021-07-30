module Common.WiringDiagram.Svg exposing (..)

import Common.WiringDiagram exposing (..)
import Common.WiringDiagram.Layout exposing (..)
import Common.WiringDiagram.Layout.Box exposing (..)
import Common.WiringDiagram.Svg.Arrow exposing (arrow)
import Element exposing (Element)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


viewSvg : Viewport -> List (Svg msg) -> Element msg
viewSvg vp svgItems =
    let
        w =
            String.fromFloat vp.width

        h =
            String.fromFloat vp.height
    in
    Element.html <|
        svg
            [ width w
            , height h
            , viewBox <| "0 0 " ++ w ++ " " ++ h
            ]
            svgItems


type alias SvgConfig a =
    { toLabelString : a -> String
    , dummy : ()
    }


layoutToSvg : Layout b -> Svg msg
layoutToSvg =
    layoutToSvgWithConfig
        { toLabelString = always "_", dummy = () }


toSvgTransform : { a | x : Float, y : Float } -> Svg.Attribute msg
toSvgTransform t =
    transform <|
        "translate("
            ++ String.fromFloat t.x
            ++ ","
            ++ String.fromFloat t.y
            ++ ")"


layoutToSvgWithConfig : SvgConfig a -> Layout a -> Svg msg
layoutToSvgWithConfig svgConfig l =
    case l of
        Group g ->
            let
                tx =
                    toSvgTransform g.transform

                itx =
                    toSvgTransform g.interiorTransform

                inner =
                    Svg.g [ itx ] <|
                        List.map layoutToSvg g.interior
            in
            case g.exterior of
                Just b ->
                    Svg.g [ tx ] <|
                        [ box svgConfig b, inner ]

                _ ->
                    inner

        Item b ->
            box svgConfig b

        Arrow arr ->
            arrow arr


diagramToSvg : Diagram a -> Svg msg
diagramToSvg d =
    layoutToSvg <| layoutDiagram d


box : SvgConfig a -> Box a -> Svg msg
box svgConfig b =
    Svg.g []
        [ Svg.rect
            [ x <| String.fromFloat b.lo.x
            , y <| String.fromFloat b.lo.y
            , width <| String.fromFloat b.width
            , height <| String.fromFloat b.height
            , rx <| String.fromFloat b.radius
            , ry <| String.fromFloat b.radius
            , fillOpacity "0.5"
            , stroke "grey"
            , strokeWidth "1"
            , strokeOpacity "0.5"
            , fill "#7da"
            ]
            []
        , case b.label of
            Just label ->
                svgBoxText
                    { x = b.lo.x + b.width / 2
                    , y = b.lo.y + b.height * 3 / 5
                    }
                    (svgConfig.toLabelString label)

            -- b.label
            -- "boxLabel"
            _ ->
                Svg.g [] []
        ]


svgBoxText : { a | x : Float, y : Float } -> String -> Svg msg
svgBoxText pos label =
    Svg.text_
        [ x <| String.fromFloat pos.x
        , y <| String.fromFloat pos.y
        , textAnchor "middle"
        , stroke "black"

        -- , strokeOpacity "0.5"
        , fontSize "16"
        , style "font-family:CiscoSansTT"

        -- , transform "scale(1, 1)"
        -- , textLength <| (String.fromInt <| 4 * r // 3) ++ "px"
        --, lengthAdjust "spacingAndGlyphs"
        ]
        [ Svg.text label ]


origin : { width : number, height : number, xMin : number, yMin : number }
origin =
    { width = 1, height = 100, xMin = 0, yMin = 0 }


smallViewport : { width : number, height : number, xMin : number, yMin : number }
smallViewport =
    { origin | width = 200, height = 200 }


wideViewport : { width : number, height : number, xMin : number, yMin : number }
wideViewport =
    { origin | width = 1200, height = 150 }


mediumViewport : { width : number, height : number, xMin : number, yMin : number }
mediumViewport =
    { origin | width = 640, height = 480 }


largeViewport : { width : number, height : number, xMin : number, yMin : number }
largeViewport =
    { origin | width = 1024, height = 768 }
