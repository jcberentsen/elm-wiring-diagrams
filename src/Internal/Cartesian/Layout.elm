module Internal.Cartesian.Layout exposing (..)

import Internal.Arrow as Arrow exposing (Arrow)
import Internal.Bound as Bound exposing (Bound)
import Internal.Cartesian as C exposing (C(..))
import Internal.Cartesian.Interface exposing (Interface(..))
import Internal.Extent as Extent exposing (Extent, Polarity(..))
import Internal.Layout.Config as Config exposing (Config)
import Internal.Vec2 as Vec2 exposing (Vec2)
import List
import List.Nonempty as NE exposing (Nonempty)


type Layout a
    = Layout
        { inArrows : Nonempty Arrow
        , wrapping : Maybe { value : a, extent : Extent }
        , contents : List (Layout a)
        , outArrows : Nonempty Arrow
        , extent : Extent
        }
    | Leaf { value : a, extent : Extent }
    | Empty


layout : Config a -> C a -> Layout a
layout config c =
    case c of
        C.Unit ->
            Empty

        -- A leaf gets some basic arrow stubs
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
                            NE.map (Arrow.truncate In) <|
                                arrowsFor In interface innerExtent Arrow.forEdge

                        outArrows =
                            NE.map (Arrow.truncate Out) <|
                                arrowsFor Out interface innerExtent Arrow.forEdge
                    in
                    horizontalWithArrows inputArrows ( inner, innerExtent ) outArrows

                _ ->
                    Empty

        -- A composition of smaller pieces
        C _ composition ->
            let
                inner =
                    composeLayout config composition

                innerBound =
                    boundOf inner
            in
            case ( inner, innerBound ) of
                ( Layout il, Just innerExtent ) ->
                    -- Line up the arrow stubs with the inner arrows
                    let
                        inputArrows =
                            alignArrowStubs In il.inArrows

                        outArrows =
                            alignArrowStubs Out il.outArrows
                    in
                    horizontalWithArrows inputArrows ( inner, innerExtent ) outArrows

                _ ->
                    Empty


alignArrowStubs : Polarity -> Nonempty Arrow -> Nonempty Arrow
alignArrowStubs polarity arrows =
    NE.map (Arrow.truncate polarity) arrows


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
                a =
                    layout config l

                b =
                    layout config r

                hz =
                    horizontal [ a, b ]

                contents =
                    List.concat <|
                        List.map2 (tie 10) hz (List.drop 1 hz)

                innerBound =
                    Bound.hull <| List.map boundOf contents
            in
            case ( contents, Bound.extentOf innerBound ) of
                ( [ Layout al, Layout bl ], Just innerExtent ) ->
                    Layout
                        { inArrows = al.inArrows
                        , wrapping = Nothing
                        , contents = contents
                        , outArrows = bl.outArrows
                        , extent = innerExtent
                        }

                _ ->
                    Empty

        C.Aside a b ->
            let
                contents =
                    vertical [ layout config a, layout config b ]

                extents =
                    NE.fromList <|
                        List.filterMap boundOf contents
            in
            case ( contents, extents ) of
                ( [ Layout al, Layout bl ], Just ne ) ->
                    Layout
                        { inArrows = NE.append al.inArrows bl.inArrows
                        , wrapping = Nothing
                        , contents = contents
                        , outArrows = NE.append al.outArrows bl.outArrows
                        , extent = Extent.hull ne
                        }

                _ ->
                    Empty

        C.Wrap label a ->
            let
                -- TODO Add the wrapping label and extend the bound
                inner =
                    layout config a

                innerBound =
                    boundOf inner

                tr =
                    Vec2 10 10

                ( contents, fullExtent, wrappingExtent ) =
                    case innerBound of
                        Just extent ->
                            let
                                wrapExtent =
                                    Extent.wrap 10 extent
                            in
                            ( translate tr inner
                            , Extent.translate tr extent |> Extent.combine wrapExtent
                            , wrapExtent
                            )

                        _ ->
                            let
                                extent =
                                    Extent.init { x = 0, y = 0 } { x = 20, y = 20 }
                            in
                            ( translate tr inner
                            , extent
                            , extent
                            )
            in
            case contents of
                Layout al ->
                    Layout
                        { inArrows = al.inArrows
                        , wrapping = Just { value = label, extent = wrappingExtent }
                        , contents = [ contents ]
                        , outArrows = al.outArrows
                        , extent = fullExtent
                        }

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


tie : Float -> Layout a -> Layout a -> List (Layout a)
tie dx a b =
    -- Connect the output ports of a to the inputs of b
    -- a and be are assumed to have been placed beside each other
    -- so the tie will wedge between them
    case ( a, b ) of
        ( Layout la, Layout lb ) ->
            let
                starts =
                    la.outArrows

                ends =
                    lb.inArrows
            in
            let
                shift =
                    { x = dx, y = 0 }

                arrows =
                    NE.map2 (Arrow.connect dx) starts ends
            in
            [ a |> updateOutArrows arrows
            , translate shift b
            ]

        _ ->
            []


updateOutArrows : Nonempty Arrow -> Layout a -> Layout a
updateOutArrows arrows l =
    case l of
        Layout ll ->
            Layout { ll | outArrows = arrows }

        _ ->
            l


{-| Layout input arrows and inner layout with outArrows

The input is assumed to be relative to the inner layout extents
and will be shifted right by the extent of the input arrows

-}
horizontalWithArrows : Nonempty Arrow -> ( Layout a, Extent ) -> Nonempty Arrow -> Layout a
horizontalWithArrows inputArrows ( inner, innerExtent ) outArrows =
    -- TODO We seem to be overusing translate here, can we simplify?
    let
        inputArrowsBound =
            Bound.hull <| List.map Arrow.boundOf <| NE.toList inputArrows

        innerShift =
            { x = Bound.width inputArrowsBound, y = 0 }

        innerShifted =
            Extent.translate innerShift innerExtent

        outputArrowsBound =
            Bound.hull <| List.map Arrow.boundOf <| NE.toList outArrows

        outputArrowsExtentList =
            case outputArrowsBound of
                Just extent ->
                    [ Extent.translate innerShift extent ]

                _ ->
                    []
    in
    Layout
        { inArrows = NE.map (Arrow.translate innerShift) <| inputArrows
        , wrapping = Nothing
        , contents = [ translate innerShift inner ]
        , outArrows = NE.map (Arrow.translate innerShift) <| outArrows
        , extent =
            case inputArrowsBound of
                Just inputArrowsExtent ->
                    Extent.hull <|
                        NE.Nonempty (Extent.translate innerShift inputArrowsExtent)
                            (innerShifted :: outputArrowsExtentList)

                _ ->
                    Extent.hull <|
                        NE.Nonempty innerShifted
                            outputArrowsExtentList
        }


translate : Vec2 -> Layout a -> Layout a
translate t ll =
    case ll of
        Layout l ->
            Layout
                { l
                    | inArrows = NE.map (Arrow.translate t) l.inArrows
                    , wrapping = Maybe.map (\w -> { w | extent = Extent.translate t w.extent }) l.wrapping
                    , contents = List.map (translate t) l.contents
                    , outArrows = NE.map (Arrow.translate t) l.outArrows
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

        shiftByBound : Bound -> List Float -> List Float
        shiftByBound b xs =
            case ( b, xs ) of
                ( Just extent, x :: _ ) ->
                    x + Extent.width extent :: xs

                ( Just extent, [] ) ->
                    [ Extent.width extent ]

                _ ->
                    [ 0 ]

        positions =
            List.reverse <| List.foldl shiftByBound [ 0 ] bounds

        extents =
            List.filterMap identity bounds

        maxHeight =
            Maybe.withDefault 0 <| List.maximum <| List.map Extent.height extents

        moveTo sub x =
            translate { x = x, y = 0 } <|
                horizontallyCentered maxHeight sub
    in
    List.map2 moveTo items positions


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

        shiftByBound : Bound -> List Float -> List Float
        shiftByBound b ys =
            case ( b, ys ) of
                ( Just extent, y :: _ ) ->
                    y + Extent.height extent :: ys

                ( Just extent, [] ) ->
                    [ Extent.height extent ]

                _ ->
                    [ 0 ]

        positions =
            List.reverse <| List.foldl shiftByBound [ 0 ] bounds

        extents =
            List.filterMap identity bounds

        maxRange =
            Maybe.withDefault 0 <|
                List.maximum <|
                    List.map Extent.width extents

        moveTo sub v =
            translate { x = 0, y = v } <| verticallyCentered maxRange sub
    in
    List.map2 moveTo items positions


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
            goExtents <|
                top

        len =
            toFloat <|
                List.length
                    extents

        average : ( List Float, List Float ) -> Vec2
        average ( xs, ys ) =
            { x = List.sum xs / len, y = List.sum ys / len }
    in
    extents |> List.map center |> List.unzip |> average


countArrows : Layout a -> Int
countArrows ll =
    case ll of
        Empty ->
            0

        Leaf _ ->
            0

        Layout l ->
            List.length (NE.toList l.inArrows)
                + List.length (NE.toList l.outArrows)
                + List.sum (List.map countArrows l.contents)


countVisibleArrows : Layout a -> Int
countVisibleArrows ll =
    case ll of
        Empty ->
            0

        Leaf _ ->
            0

        Layout l ->
            List.length (List.filter Arrow.isVisible <| NE.toList l.inArrows)
                + List.length (List.filter Arrow.isVisible <| NE.toList l.outArrows)
                + List.sum (List.map countVisibleArrows l.contents)
