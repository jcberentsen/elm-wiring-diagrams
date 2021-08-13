module Cartesian.DeepTests exposing (..)

import Expect
import Internal.Cartesian exposing (..)
import Internal.Cartesian.Interface as Interface
import Internal.Cartesian.Layout as Layout
import Internal.Layout.Config as Config
import Internal.Vec2 exposing (Vec2)
import Test exposing (..)


suite : Test
suite =
    describe "Deep cartesian structure"
        [ describe "Cartesian unit"
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
        , describe "Algebraic"
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
        , describe "Layout"
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
            , todo "Frame tag sub expressions (bubbles)"
            , todo "Sanity check extents inwards in Layout"
            , todo "Fix vertical alignment"
            , todo "Other invariants. No overlap of inner extents"
            ]
        ]


defaultConfig : Config.Config b
defaultConfig =
    Config.initWithArrowLablerAndSpacing (\_ _ -> "arrow") (Vec2 40 20)
