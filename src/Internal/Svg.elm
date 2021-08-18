module Internal.Svg exposing (..)

import Html exposing (Html)
import Internal.Svg.Config exposing (Config(..))
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (..)


{-| Placement and dimensions of a Viewport to render SVG inside
-}
type alias Viewport =
    { width : Float
    , height : Float
    , xMin : Float
    , yMin : Float
    }


{-| Render a list of Svg items in a viewport to Html
-}
view : Viewport -> List (Svg msg) -> Html msg
view vp svgItems =
    let
        w =
            String.fromFloat vp.width

        h =
            String.fromFloat vp.height
    in
    svg
        [ width w
        , height h
        , viewBox <| "0 0 " ++ w ++ " " ++ h
        ]
        svgItems


toSvgTransform : { a | x : Float, y : Float } -> Svg.Attribute msg
toSvgTransform t =
    transform <|
        "translate("
            ++ String.fromFloat t.x
            ++ ","
            ++ String.fromFloat t.y
            ++ ")"


wrap :
    Config a msg
    -> { b | label : Maybe a, lo : { c | x : Float, y : Float }, width : Float, height : Float, radius : Float }
    -> Svg msg
    -> Svg msg
wrap svgConfig b inner =
    let
        (Config config) =
            svgConfig
    in
    Svg.g (config.toBoxAttributes b.label)
        [ box svgConfig b, inner ]


box :
    Config a msg
    -> { b | lo : { c | x : Float, y : Float }, width : Float, height : Float, radius : Float, label : Maybe a }
    -> Svg msg
box (Config svgConfig) b =
    let
        rectAttributes =
            [ x <| String.fromFloat b.lo.x
            , y <| String.fromFloat b.lo.y
            , width <| String.fromFloat b.width
            , height <| String.fromFloat b.height
            , rx <| String.fromFloat b.radius
            , ry <| String.fromFloat b.radius
            , fillOpacity "0.2"
            , stroke "grey"
            , strokeWidth "1"
            , strokeOpacity "0.5"
            ]
    in
    Svg.g [] <|
        Svg.rect rectAttributes []
            :: (case b.label of
                    Just label ->
                        [ svgBoxText (svgConfig.toTextAttributes label)
                            { x = b.lo.x + b.width / 2
                            , y = b.lo.y + b.height * 3 / 5
                            }
                            (svgConfig.toLabelString label)
                        ]

                    _ ->
                        []
               )


svgBoxText :
    List (Svg.Attribute msg)
    -> { a | x : Float, y : Float }
    -> String
    -> Svg msg
svgBoxText overloadAttrs pos label =
    let
        attrs =
            [ x <| String.fromFloat pos.x
            , y <| String.fromFloat pos.y
            , textAnchor "middle"
            , stroke "black"
            , fontSize "24px"
            , pointerEvents "none"
            ]
                ++ overloadAttrs
    in
    Svg.text_ attrs
        [ Svg.text label ]


origin : { width : number, height : number, xMin : number, yMin : number }
origin =
    { width = 1, height = 100, xMin = 0, yMin = 0 }


{-| A small viewport of 200x200
-}
smallViewport : { width : number, height : number, xMin : number, yMin : number }
smallViewport =
    { origin | width = 200, height = 200 }


{-| A wide but low viewport of 1200x150
-}
wideViewport : { width : number, height : number, xMin : number, yMin : number }
wideViewport =
    { origin | width = 1200, height = 150 }


{-| A medium sized viewport (640x480)
-}
mediumViewport : { width : number, height : number, xMin : number, yMin : number }
mediumViewport =
    { origin | width = 640, height = 480 }


{-| A larger sized viewport (1024x768)
-}
largeViewport : { width : number, height : number, xMin : number, yMin : number }
largeViewport =
    { origin | width = 1024, height = 768 }
