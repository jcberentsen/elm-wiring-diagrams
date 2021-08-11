module Cartesian.DeepTests exposing (..)

import Expect
import Internal.Cartesian exposing (..)
import Internal.Cartesian.Interface as Interface
import Internal.Cartesian.Layout as Layout
import Test exposing (..)
import WiringDiagram.Layout.Config as Config
import WiringDiagram.Vec2 exposing (Vec2)


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
            , todo "Do we even need the unit?"
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
            , todo "Associativity"
            , todo "Canonical form?"
            ]
        , describe "Layout"
            [ test "Singleton item: -> a ->" <|
                \_ ->
                    let
                        expect =
                            Just
                                { lo = { x = 0, y = 0 }, hi = { x = 40, y = 20 } }
                    in
                    init "a"
                        |> Layout.layout defaultConfig
                        |> Layout.boundOf
                        |> Expect.equal expect
            , test "Singleton item center of mass" <|
                \_ ->
                    let
                        expect =
                            { x = 20, y = 10 }
                    in
                    init "a"
                        |> Layout.layout defaultConfig
                        |> Layout.centerOfMass
                        |> Expect.equal expect
            , test "No extra outer arrows in '-> a -> b ->'" <|
                \_ ->
                    let
                        sample =
                            init "a" |> before (init "b")

                        expect =
                            Just
                                { lo = { x = 0, y = 0 }, hi = { x = 80, y = 20 } }
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
            , test "Center parallel lanes vertically" <|
                \_ ->
                    let
                        sample =
                            (init "a" |> before (init "b")) |> aside (init "c")

                        expectX =
                            40
                    in
                    sample
                        |> Layout.layout defaultConfig
                        |> Layout.centerOfMass
                        |> .x
                        |> Expect.within (Expect.Absolute 0.001) expectX
            , test "Connect arrows between inner and outer" <|
                \_ ->
                    let
                        sample =
                            (init "a" |> aside (init "b")) |> before (init "c")

                        expectX =
                            0
                    in
                    sample
                        |> Layout.layout defaultConfig
                        |> Layout.centerOfMass
                        |> .x
                        |> Expect.within (Expect.Absolute 0.001) expectX
            , todo "Two arrow stubs into (2)-> box ->(1)"
            ]
        ]


defaultConfig : Config.Config b
defaultConfig =
    Config.init (\_ _ -> "arrow") (Vec2 40 20)
