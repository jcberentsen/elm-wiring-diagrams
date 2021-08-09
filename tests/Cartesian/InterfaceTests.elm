module Cartesian.InterfaceTests exposing (..)

import Expect
import Internal.Cartesian.Interface exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Cartesian interfaces"
        [ describe "Interface composition"
            [ test "a |> before b keeps unital interface (1 -> 1)" <|
                \_ ->
                    init 1 1
                        |> before (init 1 1)
                        |> Expect.equal (init 1 1)
            , test "(1->2) |> before (2->3) = (1 -> 3)" <|
                \_ ->
                    init 1 2
                        |> before (init 2 3)
                        |> Expect.equal (init 1 3)
            , test "(1->1) |> aside (1->1) = (2 -> 2)" <|
                \_ ->
                    init 1 1
                        |> aside (init 1 1)
                        |> Expect.equal (init 2 2)
            ]
        , describe "How to make invalid composition unrepresentable?"
            [ test "(1->2) |> before (10->1) = Incompatible" <|
                \_ ->
                    init 1 2
                        |> before (init 10 1)
                        |> Expect.equal (incompatibility 2 10)
            ]
        ]
