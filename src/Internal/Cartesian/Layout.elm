module Internal.Cartesian.Layout exposing (..)

import Internal.Arrow as Arrow exposing (Arrow)
import Internal.Bound as Bound exposing (Bound)
import Internal.Cartesian as C exposing (C(..))
import Internal.Cartesian.Interface exposing (Interface(..))
import Internal.Extent as Extent exposing (Extent, Polarity(..))
import Internal.WiringDiagram.Layout as L
import List.Nonempty as NE exposing (Nonempty)
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
                    boundOf inner
            in
            case Bound.extentOf innerBound of
                Just innerExtent ->
                    let
                        inputArrows =
                            arrowsFor In interface innerExtent Arrow.forEdge

                        outArrows =
                            arrowsFor Out interface innerExtent Arrow.forEdge
                    in
                    horizontalWithArrows inputArrows ( inner, innerExtent ) outArrows

                _ ->
                    Empty

        C interface composition ->
            let
                inner =
                    composeLayout config composition

                innerBound =
                    boundOf inner
            in
            case Bound.extentOf innerBound of
                Just innerExtent ->
                    let
                        arrowConfig =
                            { headLength = 0 }

                        inputArrows =
                            arrowsFor In interface innerExtent (Arrow.forEdgeWith arrowConfig)

                        outArrows =
                            arrowsFor Out interface innerExtent (Arrow.forEdgeWith arrowConfig)
                    in
                    horizontalWithArrows inputArrows ( inner, innerExtent ) outArrows

                _ ->
                    Empty


arrowsFor :
    Polarity
    -> Interface
    -> Extent
    -> (Polarity -> Int -> Extent -> Arrow)
    -> Nonempty Arrow
arrowsFor side interface innerExtent arrowPlacer =
    case interface of
        Unital ->
            NE.singleton <| arrowPlacer side 0 innerExtent

        Arity arities ->
            let
                arity =
                    case side of
                        In ->
                            arities.inp

                        Out ->
                            arities.out

                arrow n =
                    arrowPlacer side n innerExtent
            in
            NE.Nonempty (arrow 0) <| List.map arrow <| List.range 1 (arity - 1)

        _ ->
            NE.singleton <| Arrow.forEdgeWith { headLength = 2 } side 0 innerExtent


{-| Layout input arrows and inner layout with outArrows

The input is assumed to be relative to the inner layout extents
and will be shifted right by the extent of the input arrows

-}
horizontalWithArrows : Nonempty Arrow -> ( Layout a, Extent ) -> Nonempty Arrow -> Layout a
horizontalWithArrows inputArrows ( inner, innerExtent ) outArrows =
    -- TODO We seem to be overusing translate here, can we simplify?
    let
        inputArrowsExtent =
            Extent.hull <| NE.map Arrow.extentOf inputArrows

        innerShift =
            { x = Extent.width inputArrowsExtent, y = 0 }

        innerShifted =
            Extent.translate innerShift innerExtent

        outputArrowsExtent =
            Extent.hull <| NE.map Arrow.extentOf outArrows
    in
    Layout
        { inArrows = List.map (Arrow.translate innerShift) <| NE.toList inputArrows
        , contents = [ translate innerShift inner ]
        , outArrows = List.map (Arrow.translate innerShift) <| NE.toList outArrows
        , extent =
            Extent.hull <|
                NE.Nonempty (Extent.translate innerShift inputArrowsExtent)
                    [ innerShifted, Extent.translate innerShift outputArrowsExtent ]
        }


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
                    Bound.hull <| List.map boundOf contents
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
                    NE.fromList <| List.filterMap boundOf contents
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


boundOf : Layout a -> Bound
boundOf l =
    case l of
        Layout ll ->
            Just ll.extent

        Leaf ll ->
            Just ll.extent

        _ ->
            Bound.empty


{-| Back to back horizontal layout (no padding)

This will center items along a horizontal

a
-c---- center
b

-}
horizontal : List (Layout a) -> List (Layout a)
horizontal items =
    let
        bounds =
            List.map boundOf items

        extents =
            List.filterMap identity bounds

        maxHeight =
            Maybe.withDefault 0 <| List.maximum <| List.map Extent.height extents

        moveRightOf sub previousBound =
            case previousBound of
                Nothing ->
                    horizontallyCentered maxHeight sub

                Just extent ->
                    translate { x = extent.hi.x - extent.lo.x, y = 0 } <|
                        horizontallyCentered maxHeight sub
    in
    List.map2 moveRightOf items (Nothing :: bounds)


{-| Back to back vertical layout
This will center items along a horizontal

    a|b
     c

-}
vertical : List (Layout a) -> List (Layout a)
vertical items =
    let
        bounds =
            List.map boundOf items

        extents =
            List.filterMap identity bounds

        maxWidth =
            Maybe.withDefault 0 <| List.maximum <| List.map Extent.width extents

        align sub previousBound =
            case previousBound of
                Nothing ->
                    verticallyCentered maxWidth sub

                Just extent ->
                    translate { x = 0, y = extent.hi.y - extent.lo.y } <|
                        verticallyCentered maxWidth sub
    in
    List.map2 align items (Nothing :: bounds)


horizontallyCentered : Float -> Layout b -> Layout b
horizontallyCentered maxHeight sub =
    let
        toCenter v =
            Vec2 0 v

        extentSide =
            Extent.height
    in
    centerWith toCenter extentSide maxHeight sub


verticallyCentered : Float -> Layout a -> Layout a
verticallyCentered maxWidth sub =
    let
        toCenter v =
            Vec2 v 0

        extentSide =
            Extent.width
    in
    centerWith toCenter extentSide maxWidth sub


centerWith : (Float -> Vec2) -> (Extent -> Float) -> Float -> Layout b -> Layout b
centerWith toCenter extentSide range sub =
    case boundOf sub of
        Just extent ->
            let
                tr =
                    toCenter ((range - extentSide extent) / 2)
            in
            translate tr sub

        _ ->
            let
                tr =
                    toCenter (range / 2)
            in
            translate tr sub


centerOfMass : Layout a -> Vec2
centerOfMass top =
    let
        goExtents l =
            case l of
                Empty ->
                    []

                Leaf e ->
                    [ e.extent ]

                Layout sub ->
                    List.concatMap goExtents sub.contents

        center : Extent -> ( Float, Float )
        center e =
            ( (e.hi.x + e.lo.x) / 2
            , (e.lo.y + e.hi.y) / 2
            )

        extents =
            goExtents top

        len =
            toFloat <|
                List.length
                    extents

        average : ( List Float, List Float ) -> Vec2
        average ( xs, ys ) =
            { x = List.sum xs / len, y = List.sum ys / len }
    in
    extents |> List.map center |> List.unzip |> average
