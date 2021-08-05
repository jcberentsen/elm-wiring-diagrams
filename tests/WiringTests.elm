module WiringTests exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import Test exposing (..)
import WiringDiagram.Examples exposing (..)
import WiringDiagram.Internal as D exposing (..)
import WiringDiagram.Layout as Layout exposing (..)
import WiringDiagram.Layout.Box as Box
import WiringDiagram.Layout.Extent as Extent exposing (..)
import WiringDiagram.Layout.Internal as I
import WiringDiagram.Layout.Readout as Layout exposing (..)


{-| To avoid brittle layout tests, we should strive to
test invariant aspects, like centers lining up, relative placement etc.

The tests should should be invariant to things that are likely to change in the future,
like defaults for paddings, widths, heights, spacings...

Also avoid having tests overlapping in ways that one change breaks multiple tests.

Reference:
<https://github.com/statusfailed/cartographer-har>
Paper: The Cost of Compositionality: A High-Performance Implementation of String Diagram Composition
<https://arxiv.org/abs/2105.09257>

Some restrictions:
Serial composition must match on interfaces
n -> m ; m -> l

-}
suite : Test
suite =
    describe "All"
        [ describe "Diagram.alignPorts "
            [ test "align empty list of Diagrams makes little sense" <|
                \_ ->
                    alignPorts D1 [] |> Expect.equal (Just [])
            , fuzz fuzzSource "One element will be untouched" <|
                \d ->
                    alignPorts D1 [ d ] |> Expect.equal (Just [ d ])
            , describe "Sequentially D1"
                [ fuzz2 fuzzSource fuzzSource "Two sources compose to source and relay" <|
                    \sourceA sourceB ->
                        let
                            relayB =
                                sourceB |> D.map (setInPorts (outPorts sourceA))

                            expect =
                                Just [ sourceA, relayB ]
                        in
                        alignPorts D1 [ sourceA, sourceB ]
                            |> Expect.equal expect
                , fuzz sourceAndMatchingSink
                    "Matching sink aligns with source ports"
                  <|
                    \( sourceA, sinkB ) ->
                        let
                            expect =
                                sinkB
                        in
                        alignPorts D1 [ sourceA, sinkB ] |> Expect.equal (Just [ sourceA, expect ])
                , fuzz fuzzMatchingRelations "Two matching relations" <|
                    \( relA, relB ) ->
                        let
                            expect =
                                [ relA, relB ]
                        in
                        alignPorts D1 [ relA, relB ]
                            |> Expect.equal (Just expect)
                ]
            , describe "Parallel D2"
                [ test "Align ports in parallell" <|
                    \_ ->
                        let
                            expect =
                                Just [ a, b |> offsetPorts 1 ]
                        in
                        alignPorts D2 [ a, b ] |> Expect.equal expect
                ]
            ]
        , describe "Layout.computeLayout"
            [ test "can compute where to display a single box" <|
                \_ -> layoutDiagram a |> Expect.equal (I.Item <| Box.boxify a)
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
                            |> Expect.equal (Just ( Onside Left, Overlap ))
                , test "The arrow is left of B" <|
                    \_ ->
                        computedLayout
                            |> Layout.pickPair (both "arrow") (both "b")
                            |> Maybe.map Extent.compare
                            |> Expect.equal (Just ( Onside Left, Overlap ))
                ]
            , describe "sequential layout of source and sink" <|
                let
                    ac =
                        inSequence [ initWrap a, initWrap c ]

                    computedLayout =
                        layoutDiagram ac
                in
                [ test "Source is fully left of sink" <|
                    \_ ->
                        computedLayout
                            |> Layout.pickPair (both "a") (both "c")
                            |> Maybe.map Extent.compare
                            |> Expect.equal (Just ( Onside Left, Overlap ))
                , test "Two small and one connecting arrow = 3" <|
                    \_ ->
                        computedLayout
                            |> traverseArrows (always 1)
                            |> Expect.equal [ 1, 1, 1 ]
                ]
            , test "Compute default out port position" <|
                \_ ->
                    portPositionsOfBox outgoing (Box.boxify a)
                        |> Expect.equal [ ( 0, { x = 64, y = 18 } ) ]
            , test "Compute default in port position" <|
                \_ ->
                    portPositionsOfBox incoming (Box.boxify b)
                        |> Expect.equal [ ( 0, { x = 0, y = 18 } ) ]
            , test "Compute multiple port out positions" <|
                \_ ->
                    Layout.portPositionsOfBox outgoing (Box.boxify a2)
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
                            |> Expect.equal (Just ( Overlap, Onside Above ))
                , test "No arrows" <|
                    \_ ->
                        computedLayout
                            |> traverseArrows (always 1)
                            |> Expect.equal []
                ]
            , describe "More complex vertical composition" <|
                let
                    computedLayout =
                        layoutDiagram twoLanes
                in
                [ test "A is above E" <|
                    \_ ->
                        computedLayout
                            |> Layout.pickPair (both "a") (both "d")
                            |> Maybe.map Extent.compare
                            |> Expect.equal (Just ( Overlap, Onside Above ))
                , test "Inner boxes overlap outer extent" <|
                    \_ ->
                        computedLayout
                            |> Layout.pickPair (both "abde") (both "e")
                            |> Maybe.map Extent.compare
                            |> Expect.equal (Just ( Overlap, Overlap ))
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
            , test "Wrapped two parallel sources have arrows to sink" <|
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
        ]


both : a -> ( a, a )
both a =
    ( a, a )


fuzzSource : Fuzzer (Diagram String)
fuzzSource =
    Fuzz.map2 source Fuzz.string (Fuzz.intRange 1 2)


fuzzSink : Fuzzer (Diagram String)
fuzzSink =
    Fuzz.map2 sink Fuzz.string (Fuzz.intRange 1 2)


fuzzRelation : Fuzzer (Diagram String)
fuzzRelation =
    Fuzz.map3 relation Fuzz.string (Fuzz.intRange 1 2) (Fuzz.intRange 1 2)


fuzzSinkWith : Int -> Fuzzer (Diagram String)
fuzzSinkWith n =
    Fuzz.map (\label -> sink label n) Fuzz.string


sourceAndMatchingSink : Fuzzer ( Diagram String, Diagram String )
sourceAndMatchingSink =
    Fuzz.map2 (\s newLabel -> ( s, sinkForSource newLabel s )) fuzzSource Fuzz.string


fuzzMatchingRelations : Fuzzer ( Diagram String, Diagram String )
fuzzMatchingRelations =
    Fuzz.map2 (\rel newLabel -> ( rel, matchRelation newLabel rel )) fuzzRelation Fuzz.string


sinkForSource : String -> Diagram String -> Diagram String
sinkForSource label src =
    let
        interface =
            List.length (outPorts src)
    in
    sink label interface


matchRelation : String -> Diagram String -> Diagram String
matchRelation label src =
    let
        interface =
            List.length (outPorts src)
    in
    relation label interface interface
