module Internal.Cartesian.Layout.Svg exposing (layoutToSvgWithConfig)

import Internal.Arrow as Arrow exposing (Arrow(..))
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
                    case a of
                        Arrow arr ->
                            Arrow.arrow arr

                        Port p ->
                            -- maybe not?
                            Arrow.arrow <|
                                { tailPoint = p.pos
                                , adjustTail = 0
                                , adjustHead = 0
                                , headPoint = p.pos
                                }

                inArrows =
                    List.map toArrow <| l.inArrows

                outArrows =
                    List.map toArrow <| l.outArrows

                toExterior wrapping =
                    Box.fromExtent (Just wrapping.value) wrapping.extent

                exterior =
                    Maybe.map toExterior l.wrapping

                arrows =
                    inArrows ++ outArrows

                interior =
                    List.map (layoutToSvgWithConfig svgConfig) l.contents

                inners =
                    arrows ++ interior
            in
            Svg.wrap svgConfig exterior inners

        Leaf l ->
            let
                b =
                    Box.fromExtent (Just l.value) l.extent
            in
            Svg.box svgConfig b

        Empty ->
            Svg.g [] []
