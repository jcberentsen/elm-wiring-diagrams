module Cartesian.Internal exposing (C(..), Composed(..), aside, before, group, init, interface, unit)

import Cartesian.Interface as I exposing (Interface)


type C a
    = Unit
    | C Interface (Composed a)


type Composed a
    = Leaf a
    | Aside (C a) (C a)
    | Sequenced (C a) (C a)


unit : C a
unit =
    Unit


group : C a -> C a
group v =
    case v of
        Unit ->
            Unit

        _ ->
            v


init : a -> C a
init v =
    C (I.init 1 1) (Leaf v)


before : C a -> C a -> C a
before lhs rhs =
    case ( lhs, rhs ) of
        ( Unit, _ ) ->
            rhs

        ( _, Unit ) ->
            lhs

        ( C il _, C ir _ ) ->
            C (il |> I.before ir) (Sequenced lhs rhs)


aside : C a -> C a -> C a
aside lhs rhs =
    case ( lhs, rhs ) of
        ( Unit, _ ) ->
            rhs

        ( _, Unit ) ->
            lhs

        ( C il _, C ir _ ) ->
            C (il |> I.aside ir) (Aside lhs rhs)


interface : C a -> Interface
interface c =
    case c of
        Unit ->
            I.unit

        C i _ ->
            i
