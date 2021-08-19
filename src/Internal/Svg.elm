module Internal.Svg exposing (..)

import Html exposing (Html)
import Internal.Svg.Config exposing (Config(..))
import Internal.Vec2 exposing (Vec2)
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


type alias Boxy a =
    { label : Maybe a, lo : Vec2, width : Float, height : Float, radius : Float }


{-| Render a list of Svg items in a viewport to Html
-}
view : Viewport -> List (Svg msg) -> Html msg
view vp svgItems =
    let
        xlo =
            String.fromFloat vp.xMin

        ylo =
            String.fromFloat vp.yMin

        w =
            String.fromFloat vp.width

        h =
            String.fromFloat vp.height
    in
    svg
        [ width w
        , height h
        , viewBox <| String.join " " <| [ xlo, ylo, w, h ]
        ]
        svgItems


toSvgTransform : Vec2 -> Svg.Attribute msg
toSvgTransform t =
    transform <|
        "translate("
            ++ String.fromFloat t.x
            ++ ","
            ++ String.fromFloat t.y
            ++ ")"


wrap :
    Config a msg
    -> Maybe (Boxy a)
    -> List (Svg msg)
    -> Svg msg
wrap svgConfig exterior inners =
    let
        (Config config) =
            svgConfig
    in
    case exterior of
        Just b ->
            config.wrapFunction b.label <|
                box svgConfig b
                    :: inners

        _ ->
            config.wrapFunction Nothing inners


box :
    Config a msg
    -> Boxy a
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
                ++ svgConfig.toBoxAttributes b.label
    in
    Svg.g [] <|
        Svg.rect rectAttributes []
            :: (case b.label of
                    Just label ->
                        let
                            textPosition =
                                svgConfig.labelPosition b
                        in
                        [ svgBoxText (svgConfig.toTextAttributes label)
                            textPosition
                            (svgConfig.toLabelString label)
                        ]

                    _ ->
                        []
               )


svgBoxText :
    List (Svg.Attribute msg)
    -> Vec2
    -> String
    -> Svg msg
svgBoxText overloadAttrs pos label =
    let
        attrs =
            [ x <| String.fromFloat pos.x
            , y <| String.fromFloat pos.y
            , textAnchor "middle"
            , dominantBaseline "middle"
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
