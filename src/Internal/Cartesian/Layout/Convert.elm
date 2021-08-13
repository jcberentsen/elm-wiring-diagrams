module Internal.Cartesian.Layout.Convert exposing (finalizeLayout)

import Internal.Arrow as Arrow
import Internal.Cartesian.Layout exposing (..)
import Internal.Vec2 exposing (Vec2)
import Internal.WiringDiagram.Layout as L
import List
import List.Nonempty as NE
import WiringDiagram.Layout.Box as Box


finalizeLayout : Layout a -> L.Layout a
finalizeLayout cl =
    case cl of
        Layout l ->
            let
                toArrow a =
                    let
                        ( t, h ) =
                            Arrow.ends a
                    in
                    L.Arrow
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
            in
            L.Group
                { transform = Vec2 0 0
                , exterior = Maybe.map toExterior l.wrapping
                , interiorTransform = Vec2 0 0
                , interior = NE.toList inArrows ++ List.map finalizeLayout l.contents ++ NE.toList outArrows
                }

        Leaf l ->
            L.Item <| Box.fromExtent (Just l.value) l.extent

        Empty ->
            L.empty
