module Cartesian.DeepTests exposing (suite)

import Expect
import Internal.Arrow as Arrow exposing (Arrow(..), Meander(..))
import Internal.Cartesian exposing (..)
import Internal.Cartesian.Interface as Interface
import Internal.Cartesian.Layout as Layout
import Internal.Extent exposing (Polarity(..))
import Internal.Layout.Config as Config
import Internal.Svg.Arrow exposing (computeArrowDetails)
import Internal.Vec2 exposing (Vec2)
import Test exposing (..)


suite : Test
suite =
    describe "Deep cartesian structure"
        [ cartesianUnitTests
        , algebraicTests
        , layoutTests
        , arrowConnectTests
        ]


cartesianUnitTests : Test
cartesianUnitTests =
    describe "Cartesian unit"
        [ test "Can be identified" <|
            \_ ->
                unit
                    |> Expect.equal unit
        , test "Grouping a unit is unit" <|
            \_ ->
                group unit
                    |> Expect.equal unit
        , test "a ; I = a" <|
            \_ ->
                let
                    lhs =
                        init "a"
                in
                lhs
                    |> before unit
                    |> Expect.equal lhs
        , test "I |> before a = a" <|
            \_ ->
                let
                    rhs =
                        init "a"
                in
                unit
                    |> before rhs
                    |> Expect.equal rhs
        , test "I |> aside a = a" <|
            \_ ->
                let
                    rhs =
                        init "a"
                in
                unit
                    |> aside rhs
                    |> Expect.equal rhs
        , test "a |> aside I = a" <|
            \_ ->
                let
                    lhs =
                        init "a"
                in
                lhs
                    |> aside unit
                    |> Expect.equal lhs
        ]


algebraicTests : Test
algebraicTests =
    describe "Algebraic"
        [ test "a |> before b keeps interface (1 -> 1)" <|
            \_ ->
                init "a"
                    |> before (init "b")
                    |> interface
                    |> Expect.equal (Interface.init 1 1)
        , test "a |> aside b becomes (2 -> 2)" <|
            \_ ->
                init "a"
                    |> aside (init "b")
                    |> interface
                    |> Expect.equal (Interface.init 2 2)
        ]


layoutTests : Test
layoutTests =
    describe "Layout"
        [ describe "Singleton a"
            [ test "Extent should match configuration 20x20" <|
                \_ ->
                    let
                        expect =
                            Just
                                { lo = { x = 0, y = 0 }, hi = { x = 20, y = 20 } }
                    in
                    init "a"
                        |> Layout.layout defaultConfig
                        |> Layout.boundOf
                        |> Expect.equal expect
            , test "Center of mass is 10,10" <|
                \_ ->
                    let
                        expect =
                            { x = 10, y = 10 }
                    in
                    init "a"
                        |> Layout.layout defaultConfig
                        |> Layout.centerOfMass
                        |> Expect.equal expect
            , test "Wrapping shifts center of mass by wrapping border" <|
                \_ ->
                    let
                        expect =
                            Just
                                { lo = { x = 0, y = 0 }, hi = { x = 40, y = 40 } }
                    in
                    init "a"
                        |> wrap "wrapping"
                        |> Layout.layout defaultConfig
                        |> Layout.boundOf
                        |> Expect.equal expect
            ]
        , test "No extra outer arrows in 'a -> b'" <|
            \_ ->
                let
                    sample =
                        init "a" |> before (init "b")

                    expect =
                        Just
                            { lo = { x = 0, y = 0 }, hi = { x = 50, y = 20 } }
                in
                sample
                    |> Layout.layout defaultConfig
                    |> Layout.boundOf
                    |> Expect.equal expect
        , test "Center y horizontally" <|
            \_ ->
                let
                    sample =
                        (init "a" |> aside (init "b")) |> before (init "c")

                    expectY =
                        20
                in
                sample
                    |> Layout.layout defaultConfig
                    |> Layout.centerOfMass
                    |> .y
                    |> Expect.within (Expect.Absolute 0.001) expectY
        , test "Horizontal center of mass" <|
            \_ ->
                let
                    sample =
                        init "a" |> before (init "b")

                    expectX =
                        25
                in
                sample
                    |> Layout.layout defaultConfig
                    |> Layout.centerOfMass
                    |> .x
                    |> Expect.within (Expect.Absolute 0.001) expectX
        , test "Center parallel lanes vertically" <|
            \_ ->
                let
                    sample =
                        (init "a" |> before (init "b")) |> aside (init "c")

                    expectX =
                        25
                in
                sample
                    |> Layout.layout defaultConfig
                    |> Layout.centerOfMass
                    |> .x
                    |> Expect.within (Expect.Absolute 0.001) expectX
        , test "(a x b) => c should have 2 visible arrows" <|
            \_ ->
                let
                    sample =
                        (init "a"
                            |> aside (init "b")
                        )
                            |> before (initWith (Interface.init 2 1) "c")
                in
                sample
                    |> Layout.layout defaultConfig
                    |> Layout.countVisibleArrows
                    |> Expect.equal 2
        , todo "C.Layout.Svg directly"
        , todo "Vertical spacing"
        , todo "Horizontal spacing"
        , todo "Arrow sizing"
        , todo "Wrapper padding"
        , todo "Frame tag sub expressions (bubbles)"
        , todo "Sanity check extents inwards in Layout"
        , todo "Fix vertical alignment"
        , todo "Other invariants. No overlap of inner extents"
        ]


arrowConnectTests : Test
arrowConnectTests =
    describe "Arrow connecting" <|
        let
            source =
                { pos = Vec2 0 0
                , polarity = Out
                , adjust = 0
                }

            sink =
                { pos = Vec2 0 0
                , polarity = In
                , adjust = 0
                }
        in
        [ test "Connect two Ports for a complete Arrow" <|
            \_ ->
                let
                    expected =
                        Arrow
                            { tailPoint = Vec2 0 0
                            , adjustTail = 0
                            , meander = Direct
                            , adjustHead = 0
                            , headPoint = Vec2 1 0
                            }
                in
                Arrow.connect 1 (Port source) (Port sink)
                    |> Expect.equal expected
        , test "Connect two Ports with tail adjustment leftward" <|
            \_ ->
                let
                    expected =
                        Arrow
                            { tailPoint = Vec2 0 0
                            , adjustTail = -0.2
                            , meander = Direct
                            , adjustHead = 0
                            , headPoint = Vec2 1 0
                            }
                in
                Arrow.connect 1
                    (Port { source | adjust = -0.2 })
                    (Port sink)
                    |> Expect.equal expected
        , test "Connect two Ports with head adjustment leftward" <|
            \_ ->
                let
                    expected =
                        Arrow
                            { tailPoint = Vec2 0 0
                            , adjustTail = 0
                            , meander = Direct
                            , adjustHead = -0.2
                            , headPoint = Vec2 1 0
                            }
                in
                Arrow.connect 1
                    (Port source)
                    (Port { sink | adjust = -0.2 })
                    |> Expect.equal expected
        , test "We can compute waypoints for straight arrow" <|
            \_ ->
                let
                    arrow =
                        { tailPoint = Vec2 0 0
                        , adjustTail = 0
                        , meander = Direct
                        , adjustHead = 0
                        , headPoint = Vec2 1 0
                        }

                    expected =
                        { start = ( 0, 0 )
                        , ascent = ( 0.5, 0 )
                        , descent = ( 0.5, 0 )
                        , end = ( 1, 0 )
                        , headLeft = ( 0.75, -0.25 )
                        , headRight = ( 0.75, 0.25 )
                        }
                in
                computeArrowDetails arrow
                    |> Expect.equal expected
        , test "We can compute waypoints for bent arrow" <|
            \_ ->
                let
                    arrow =
                        { tailPoint = Vec2 0 0
                        , adjustTail = 0
                        , meander = Direct
                        , adjustHead = 0
                        , headPoint = Vec2 1 1
                        }

                    expected =
                        { start = ( 0, 0 )
                        , ascent = ( 0.5, 0 )
                        , descent = ( 0.5, 1 )
                        , end = ( 1, 1 )
                        , headLeft = ( 0.75, 0.75 )
                        , headRight = ( 0.75, 1.25 )
                        }
                in
                computeArrowDetails arrow
                    |> Expect.equal expected
        , test "We can compute waypoints for arrow needing initial boost" <|
            \_ ->
                let
                    arrow =
                        { tailPoint = Vec2 0 0
                        , adjustTail = 0.2
                        , meander = Direct
                        , adjustHead = 0
                        , headPoint = Vec2 1 1
                        }

                    expected =
                        { start = ( 0, 0 )
                        , ascent = ( 0.6, 0 )
                        , descent = ( 0.6, 1 )
                        , end = ( 1, 1 )
                        , headLeft = ( 0.75, 0.75 )
                        , headRight = ( 0.75, 1.25 )
                        }
                in
                computeArrowDetails arrow
                    |> Expect.all
                        [ .ascent >> Tuple.first >> Expect.within (Expect.Absolute 0.0001) (Tuple.first expected.ascent)
                        , .ascent >> Tuple.second >> Expect.within (Expect.Absolute 0.0001) (Tuple.second expected.ascent)
                        ]
        , test "Equally squeezed arrow gets normal waypoints?" <|
            \_ ->
                let
                    arrow =
                        { tailPoint = Vec2 0 0
                        , adjustTail = 0.3
                        , meander = Direct
                        , adjustHead = -0.3
                        , headPoint = Vec2 1 1
                        }

                    expected =
                        { start = ( 0, 0 )
                        , ascent = ( 0.5, 0 )
                        , descent = ( 0.5, 1 )
                        , end = ( 1, 1 )
                        , headLeft = ( 0.75, 0.75 )
                        , headRight = ( 0.75, 1.25 )
                        }
                in
                computeArrowDetails arrow
                    |> Expect.equal expected
        ]


defaultConfig : Config.Config b
defaultConfig =
    let
        spacing =
            Vec2 0 0
    in
    Config.initWithArrowLablerAndSpacing (\_ _ -> "arrow") spacing
