module Internal.Cartesian.Layout.Svg exposing (layoutToSvgWithConfig)

import Internal.Arrow as Arrow
import Internal.Box as Box
import Internal.Cartesian.Layout exposing (..)
import Internal.Svg as Svg
import Internal.Svg.Arrow as Arrow
import Internal.Svg.Config as Svg
import List
import List.Nonempty as NE
import Svg exposing (Svg)


{-| Render a Layout to Svg with configurable labeling
-}
layoutToSvgWithConfig : Svg.Config a msg -> Layout a -> Svg msg
layoutToSvgWithConfig svgConfig cl =
    case cl of
        Layout l ->
            let
                toArrow a =
                    let
                        ( t, h ) =
                            Arrow.ends a
                    in
                    Arrow.arrow <|
                        { label = ""
                        , tail = t
                        , head = h
                        }

                inArrows =
                    NE.map toArrow <| l.inArrows

                outArrows =
                    NE.map toArrow <| l.outArrows

                toExterior wrapping =
                    Box.fromExtent (Just wrapping.value) wrapping.extent

                exterior =
                    Maybe.map toExterior l.wrapping

                arrows =
                    NE.toList inArrows ++ NE.toList outArrows

                interior =
                    List.map (layoutToSvgWithConfig svgConfig) l.contents
            in
            let
                inner =
                    Svg.g [] <| arrows ++ interior
            in
            case exterior of
                Just b ->
                    Svg.g [] <|
                        [ Svg.box svgConfig b, inner ]

                _ ->
                    Svg.g [] [ inner ]

        Leaf l ->
            let
                b =
                    Box.fromExtent (Just l.value) l.extent
            in
            Svg.box svgConfig b

        Empty ->
            Svg.g [] []
