module WiringTests exposing (..)

import Expect
import Fuzz exposing (int, list)
import Test exposing (..)
import WiringDiagram as D exposing (..)
import WiringDiagram.Layout as Layout exposing (..)
import WiringDiagram.Layout.Box as Box
import WiringDiagram.Layout.Extent as Extent
import WiringDiagram.Layout.Readout as Layout exposing (..)


{-| To avoid brittle layout tests, we should strive to
test invariant aspects, like centers lining up, relative placement etc.

The tests should should be invariant to things that are likely to change in the future,
like defaults for paddings, widths, heights, spacings...

Also avoid having tests overlapping in ways that one change breaks multiple tests.

-}
suite : Test
suite =
    let
        a =
            D.source "a" 1

        b =
            D.sink "b" 1

        a2 =
            D.source "a2" 2

        b2 =
            D.sink "b2" 2
    in
    describe "computeLayout"
        [ test "can compute where to display a single box" <|
            \_ -> layoutDiagram a |> Expect.equal (Item <| Box.boxify a)
        , describe "layout two sequential boxes" <|
            let
                ab =
                    inSequence [ a, b ]

                computedLayout =
                    layoutDiagram ab
            in
            [ test "A is fully left of B" <|
                \_ ->
                    computedLayout
                        |> Layout.pickPair (both "a") (both "b")
                        |> Maybe.map Extent.compare
                        |> Expect.equal (Just Extent.LeftOf)
            , test "The arrow is left of B" <|
                \_ ->
                    computedLayout
                        |> Layout.pickPair (both "arrow") (both "b")
                        |> Maybe.map Extent.compare
                        |> Expect.equal (Just Extent.LeftOf)
            ]
        , describe "layout of two sequential sub diagrams" <|
            let
                ab =
                    inSequence [ initWrap a, initWrap b ]

                computedLayout =
                    layoutDiagram ab
            in
            [ test "A is fully left of B" <|
                \_ ->
                    computedLayout
                        |> Layout.pickPair (both "a") (both "b")
                        |> Maybe.map Extent.compare
                        |> Expect.equal (Just Extent.LeftOf)
            , test "Two small and one connecting arrow = 3" <|
                \_ ->
                    computedLayout
                        |> traverseArrows (always 1)
                        |> Expect.equal [ 1, 1, 1 ]
            ]
        , test "Compute default out port position" <|
            \_ ->
                portPositionsOfBox Out (Box.boxify a)
                    |> Expect.equal [ ( 0, { x = 64, y = 18 } ) ]
        , test "Compute default in port position" <|
            \_ ->
                portPositionsOfBox In (Box.boxify b)
                    |> Expect.equal [ ( 0, { x = 0, y = 18 } ) ]
        , test "Compute multiple port out positions" <|
            \_ ->
                Layout.portPositionsOfBox Out (Box.boxify a2)
                    |> Expect.equal
                        [ ( 0, { x = 64, y = 12 } )
                        , ( 1, { x = 64, y = 24 } )
                        ]
        , test "Connect boxes with two ports" <|
            \_ ->
                let
                    ab =
                        inSequence [ a2, b2 ]

                    computedLayout =
                        layoutDiagram ab
                in
                computedLayout
                    |> traverseArrows (always 1)
                    |> List.length
                    |> Expect.equal 2
        , describe "Compose vertically" <|
            let
                ab =
                    inParallel [ a, b ]

                computedLayout =
                    layoutDiagram ab
            in
            [ test "A is above B" <|
                \_ ->
                    computedLayout
                        |> Layout.pickPair (both "a") (both "b")
                        |> Maybe.map Extent.compare
                        |> Expect.equal (Just Extent.Overlapping)
            , test "No arrows" <|
                \_ ->
                    computedLayout
                        |> traverseArrows (always 1)
                        |> Expect.equal []
            ]
        , describe "Wrapped box" <|
            let
                exteriorA =
                    initWrap a

                computedLayout =
                    layoutDiagram exteriorA
            in
            [ test "Has arrow to exterior" <|
                \_ ->
                    computedLayout
                        |> traverseArrows (always 1)
                        |> Expect.equal
                            [ 1 ]
            , test "The arrow is centered and straight" <|
                \_ ->
                    let
                        arrCY =
                            Maybe.map Extent.computeCenterY <| findExtent (both "arrow") computedLayout

                        aCY =
                            Maybe.map Extent.computeCenterY <| findExtent (both "a") computedLayout
                    in
                    Expect.equal arrCY aCY
            ]
        , test "Wrapping an element lifts unconnected ports" <|
            \_ ->
                initWrap (source "a" 1) |> D.outPorts |> Expect.equal [ 0 ]
        , test "Two arrows to exterior" <|
            \_ ->
                inSequence
                    [ initWrap (source "a" 2)
                    ]
                    |> layoutDiagram
                    |> traverseArrows (always 1)
                    |> Expect.equal [ 1, 1 ]
        , test "Two internal outgoing plus two exterior connecting arrows" <|
            \_ ->
                inSequence
                    [ initWrap (source "a" 2)
                    , sink "b" 2
                    ]
                    |> layoutDiagram
                    |> traverseArrows identity
                    |> List.length
                    |> Expect.equal 4
        , test "Wrapped two paralell sources have arrows to sink" <|
            \_ ->
                inSequence
                    [ initWrap <|
                        inParallel
                            [ source "a" 1
                            , source "b" 1 |> offsetPorts 1
                            ]
                    , sink "c" 2
                    ]
                    |> layoutDiagram
                    |> traverseArrows identity
                    |> List.length
                    |> Expect.equal 4
        , test "Center boxes vertically ab -> c" <|
            \_ ->
                inSequence
                    [ initWrap <| source "a" 1
                    , sink "b" 1
                    ]
                    |> layoutDiagram
                    |> Layout.pickPair (both "a") (both "b")
                    |> Maybe.map (Tuple.mapBoth Extent.computeCenterY Extent.computeCenterY)
                    |> Expect.equal (Just ( 28, 28 ))
        , describe "Alter layout config"
            [ todo "More horizontal spacing"
            , todo "More vertical spacing"
            , todo "Bigger boxes"
            ]
        ]


both : a -> ( a, a )
both a =
    ( a, a )
