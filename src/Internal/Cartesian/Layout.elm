module Internal.Cartesian.Layout exposing (..)

import Internal.Arrow as Arrow exposing (Arrow)
import Internal.Bound as Bound exposing (Bound)
import Internal.Cartesian as C exposing (C(..))
import Internal.Cartesian.Interface as I
import Internal.Extent as Extent exposing (Extent)
import Internal.WiringDiagram.Layout as L
import List.Nonempty as NE
import WiringDiagram.Layout.Box as Box
import WiringDiagram.Layout.Config as Config exposing (Config)
import WiringDiagram.Vec2 as Vec2 exposing (Vec2)


type Layout a
    = Layout
        { inArrows : List Arrow
        , contents : List (Layout a)
        , outArrows : List Arrow
        , extent : Extent
        }
    | Leaf { value : a, extent : Extent }
    | Empty


layout : Config a -> C a -> Layout a
layout config c =
    case c of
        C.Unit ->
            Empty

        C interface (C.Leaf l) ->
            let
                inner =
                    composeLayout config (C.Leaf l)

                innerBound =
                    extentOf inner
            in
            case Bound.extentOf innerBound of
                Just innerExtent ->
                    let
                        inputArrow =
                            -- case interface of
                            --     _ ->
                            Arrow.intoLeftEdge innerExtent

                        inputArrowsExtent =
                            Arrow.extentOf inputArrow

                        shift =
                            { x = inputArrowsExtent.hi.x - inputArrowsExtent.lo.x, y = 0 }

                        innerShifted =
                            Extent.translate shift innerExtent

                        outArrow =
                            -- case interface of
                            --     _ ->
                            Arrow.outRightEdge innerShifted
                    in
                    Layout
                        { inArrows = [ inputArrow ]
                        , contents = [ translate shift inner ]
                        , outArrows = [ outArrow ]
                        , extent = Extent.combine (Extent.combine inputArrowsExtent innerShifted) <| Arrow.extentOf outArrow
                        }

                _ ->
                    Empty

        C interface composition ->
            let
                inner =
                    composeLayout config composition

                innerBound =
                    extentOf inner
            in
            case Bound.extentOf innerBound of
                Just innerExtent ->
                    let
                        inputArrow =
                            -- case interface of
                            --     _ ->
                            Arrow.intoLeftEdge innerExtent

                        inputArrowsExtent =
                            Arrow.extentOf inputArrow

                        shift =
                            { x = inputArrowsExtent.hi.x - inputArrowsExtent.lo.x, y = 0 }

                        innerShifted =
                            Extent.translate shift innerExtent

                        outArrow =
                            -- case interface of
                            --     _ ->
                            Arrow.outRightEdge innerShifted
                    in
                    Layout
                        { inArrows = []
                        , contents = [ inner ]
                        , outArrows = []
                        , extent = innerExtent -- Extent.combine (Extent.combine inputArrowsExtent innerShifted) <| Arrow.extentOf outArrow
                        }

                _ ->
                    Empty



-- C interface composition ->
--     let
--         inner =
--             composeLayout config composition
--         innerBound =
--             extentOf inner
--     in
--     case Bound.extentOf innerBound of
--         Just innerExtent ->
--             let
--                 inputArrow =
--                     -- case interface of
--                     --     _ ->
--                     Arrow.intoLeftEdge innerExtent
--                 inputArrowsExtent =
--                     Arrow.extentOf inputArrow
--                 shift =
--                     { x = inputArrowsExtent.hi.x - inputArrowsExtent.lo.x, y = 0 }
--                 innerShifted =
--                     Extent.translate shift innerExtent
--                 outArrow =
--                     -- case interface of
--                     --     _ ->
--                     Arrow.outRightEdge innerShifted
--             in
--             Layout
--                 { inArrows = [ inputArrow ]
--                 , contents = [ translate shift inner ]
--                 , outArrows = [ outArrow ]
--                 , extent = Extent.combine (Extent.combine inputArrowsExtent innerShifted) <| Arrow.extentOf outArrow
--                 }
--         _ ->
--             Empty


composeLayout : Config a -> C.Composed a -> Layout a
composeLayout config c =
    case c of
        C.Leaf a ->
            let
                bound =
                    Config.leafExtent config a
            in
            case Bound.extentOf bound of
                Just innerExtent ->
                    Leaf { value = a, extent = innerExtent }

                _ ->
                    Empty

        C.Sequenced l r ->
            let
                contents =
                    horizontal [ layout config l, layout config r ]

                innerBound =
                    Bound.hull <| List.map extentOf contents
            in
            case Bound.extentOf innerBound of
                Just innerExtent ->
                    Layout
                        { inArrows = []
                        , contents = contents
                        , outArrows = []
                        , extent = innerExtent
                        }

                _ ->
                    Empty

        C.Aside a b ->
            let
                contents =
                    vertical [ layout config a, layout config b ]

                extents =
                    NE.fromList <| List.filterMap extentOf contents
            in
            case extents of
                Just ne ->
                    Layout
                        { inArrows = []
                        , contents = contents
                        , outArrows = []
                        , extent = Extent.hull ne
                        }

                _ ->
                    Empty


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
                    List.map toArrow <| l.inArrows

                outArrows =
                    List.map toArrow <| l.outArrows
            in
            L.Group
                { transform = Vec2 0 0
                , exterior = Nothing
                , interiorTransform = Vec2 0 0
                , interior = inArrows ++ List.map finalizeLayout l.contents ++ outArrows
                }

        Leaf l ->
            L.Item <| Box.fromExtent (Just l.value) l.extent

        Empty ->
            L.empty


translate : Vec2 -> Layout a -> Layout a
translate t ll =
    case ll of
        Layout l ->
            Layout
                { l
                    | inArrows = List.map (Arrow.translate t) l.inArrows
                    , contents = List.map (translate t) l.contents
                    , outArrows = List.map (Arrow.translate t) l.outArrows
                    , extent = Extent.map (Vec2.translate t) l.extent
                }

        Leaf l ->
            Leaf { l | extent = Extent.map (Vec2.translate t) l.extent }

        Empty ->
            Empty


extentOf : Layout a -> Bound
extentOf l =
    case l of
        Layout ll ->
            Just ll.extent

        Leaf ll ->
            Just ll.extent

        _ ->
            Bound.empty


{-| Back to back horizontal layout (no padding)
-}
horizontal : List (Layout a) -> List (Layout a)
horizontal items =
    let
        extents =
            Nothing :: List.map extentOf items

        moveRightOf i previousBound =
            case previousBound of
                Nothing ->
                    i

                Just extent ->
                    translate { x = extent.hi.x - extent.lo.x, y = 0 } i
    in
    List.map2 moveRightOf items extents


{-| Back to back horizontal layout (no padding)
-}
horizontalFixed : List (Layout a) -> List (Layout a)
horizontalFixed items =
    let
        hmove n item =
            translate { x = toFloat n * 40, y = 0 } item
    in
    List.indexedMap hmove items


{-| Back to back vertical layout (no padding)
-}
vertical : List (Layout a) -> List (Layout a)
vertical items =
    let
        vmove n item =
            translate { x = 0, y = toFloat n * 40 } item
    in
    List.indexedMap vmove items



-- L.parLayouts config <| List.map (toLayoutWithConfig config) [ a, b ]
