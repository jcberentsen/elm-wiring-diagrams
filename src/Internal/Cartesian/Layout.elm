module Internal.Cartesian.Layout exposing (..)

import Internal.Arrow as Arrow exposing (Arrow)
import Internal.Bound as Bound exposing (Bound)
import Internal.Cartesian as C exposing (C(..))
import Internal.Cartesian.Interface exposing (Interface(..))
import Internal.Extent as Extent exposing (Extent, Polarity(..))
import Internal.Layout.Config as Config exposing (Config(..))
import Internal.Vec2 as Vec2 exposing (Vec2)
import List
import List.Nonempty as NE exposing (Nonempty)


type Layout a
    = Layout
        { inArrows : List Arrow
        , wrapping : Maybe { value : a, extent : Extent }
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
                        -- Start Arrow builder stubs
                        inputArrows =
                            stubsFor In interface innerExtent Arrow.stubForEdge

                        outArrows =
                            stubsFor Out interface innerExtent Arrow.stubForEdge
                    in
                    -- TODO Since the arrows are just points, we won't need to care about their extents here
                    Layout
                        { inArrows = inputArrows
                        , wrapping = Nothing
                        , contents = [ inner ]
                        , outArrows = outArrows
                        , extent = innerExtent
                        }

                _ ->
                    Empty

        -- A composition of smaller pieces
        C _ composition ->
            composeLayout config composition


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
                pair =
                    horizontal config
                        ( layout config l
                        , layout config r
                        )

                contents =
                    tie 10 pair

                extents =
                    Maybe.map (Tuple.mapBoth boundOf boundOf) contents
            in
            case ( contents, extents ) of
                ( Just ( Layout al, Layout bl ), Just ( Just extentA, Just extentB ) ) ->
                    let
                        hull =
                            Extent.hull <| NE.Nonempty extentA [ extentB ]

                        wallX polarity =
                            case polarity of
                                In ->
                                    hull.lo.x

                                Out ->
                                    hull.hi.x

                        safe polarity arrow =
                            Arrow.safeTo polarity
                                (wallX polarity)
                                arrow

                        safeArrows polarity arrows =
                            List.map (safe polarity) arrows
                    in
                    Layout
                        { inArrows = safeArrows In al.inArrows
                        , wrapping = Nothing
                        , contents = [ Layout al, Layout bl ]
                        , outArrows = safeArrows Out bl.outArrows
                        , extent = hull
                        }

                _ ->
                    Empty

        C.Aside a b ->
            let
                contents =
                    vertical config ( layout config a, layout config b )

                extents =
                    Tuple.mapBoth boundOf boundOf contents
            in
            case ( contents, extents ) of
                ( ( Layout al, Layout bl ), ( Just extentA, Just extentB ) ) ->
                    let
                        hull =
                            Extent.hull <| NE.Nonempty extentA [ extentB ]

                        displaceToAvoidCorner polarity selfExtent =
                            case polarity of
                                In ->
                                    hull.lo.x - selfExtent.lo.x

                                Out ->
                                    hull.hi.x - selfExtent.hi.x

                        safe polarity selfExtent arrow =
                            Arrow.safe polarity
                                (Vec2 (displaceToAvoidCorner polarity selfExtent) 0)
                                arrow

                        safeArrows polarity selfExtent arrows =
                            List.map (safe polarity selfExtent) arrows
                    in
                    Layout
                        { inArrows = List.append (safeArrows In extentA al.inArrows) (safeArrows In extentB bl.inArrows)
                        , wrapping = Nothing
                        , contents = [ Tuple.first contents, Tuple.second contents ]
                        , outArrows = List.append (safeArrows Out extentA al.outArrows) (safeArrows Out extentB bl.outArrows)
                        , extent = hull
                        }

                _ ->
                    Empty

        C.Wrap label a ->
            let
                inner =
                    layout config a

                configuredBound =
                    let
                        (Config cc) =
                            config
                    in
                    cc.leafExtent label

                innerBound =
                    Bound.hull [ configuredBound, boundOf inner ]

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


stubsFor :
    Polarity
    -> Interface
    -> Extent
    -> (Polarity -> Int -> Int -> Extent -> Arrow)
    -> List Arrow
stubsFor polarity interface innerExtent arrowPlacer =
    List.map (Arrow.truncate polarity) <|
        case interface of
            Unital ->
                List.singleton <| arrowPlacer polarity 0 1 innerExtent

            Arity arities ->
                let
                    arity =
                        case polarity of
                            In ->
                                arities.inp

                            Out ->
                                arities.out

                    arrow n =
                        arrowPlacer polarity n arity innerExtent
                in
                List.map arrow <| List.range 0 (arity - 1)

            _ ->
                List.singleton <| Arrow.forEdgeWith { headLength = 2 } polarity 0 1 innerExtent


tie : Float -> ( Layout a, Layout a ) -> Maybe ( Layout a, Layout a )
tie dx ( a, b ) =
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

                shift =
                    { x = dx, y = 0 }

                arrows =
                    List.map2 (Arrow.connect dx) starts ends
            in
            Just
                ( a |> updateOutArrows arrows
                , translate shift b
                )

        -- Leaf and Empty cannot tie to anything, as they have no interface (Arrows)
        _ ->
            Nothing


updateOutArrows : List Arrow -> Layout a -> Layout a
updateOutArrows arrows l =
    case l of
        Layout ll ->
            Layout { ll | outArrows = arrows }

        _ ->
            l


translate : Vec2 -> Layout a -> Layout a
translate t ll =
    case ll of
        Layout l ->
            Layout
                { l
                    | inArrows = List.map (Arrow.translate t) l.inArrows
                    , wrapping = Maybe.map (\w -> { w | extent = Extent.translate t w.extent }) l.wrapping
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
horizontal : Config a -> ( Layout a, Layout a ) -> ( Layout a, Layout a )
horizontal (Config config) ( a, b ) =
    let
        boundA =
            boundOf a

        boundB =
            boundOf b

        shift =
            case boundA of
                Just extent ->
                    Extent.width extent + config.spacing.x

                _ ->
                    0

        extents =
            List.filterMap identity [ boundA, boundB ]

        maxHeight =
            Maybe.withDefault 0 <| List.maximum <| List.map Extent.height extents

        moveTo sub x =
            translate { x = x, y = 0 } <|
                horizontallyCentered maxHeight sub
    in
    ( moveTo a 0, moveTo b shift )


{-| Back to back vertical layout
This will center items along a horizontal

    a|b
     c

-}
vertical : Config a -> ( Layout a, Layout a ) -> ( Layout a, Layout a )
vertical (Config config) ( a, b ) =
    let
        boundA =
            boundOf a

        boundB =
            boundOf b

        shift =
            case boundA of
                Just extent ->
                    Extent.height extent + config.spacing.y

                _ ->
                    0

        extents =
            List.filterMap identity [ boundA, boundB ]

        maxRange =
            Maybe.withDefault 0 <|
                List.maximum <|
                    List.map Extent.width extents

        moveTo sub v =
            translate { x = 0, y = v } <| verticallyCentered maxRange sub
    in
    ( moveTo a 0, moveTo b shift )


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
            List.length l.inArrows
                + List.length l.outArrows
                + List.sum (List.map countArrows l.contents)


countVisibleArrows : Layout a -> Int
countVisibleArrows ll =
    case ll of
        Empty ->
            0

        Leaf _ ->
            0

        Layout l ->
            List.length (List.filter Arrow.isVisible <| l.inArrows)
                + List.length (List.filter Arrow.isVisible <| l.outArrows)
                + List.sum (List.map countVisibleArrows l.contents)
