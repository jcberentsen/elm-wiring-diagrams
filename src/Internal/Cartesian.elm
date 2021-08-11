module Internal.Cartesian exposing (C(..), Composed(..), aside, before, group, init, initWith, interface, unit)

import Internal.Cartesian.Interface as I exposing (Interface)


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


initWith : Interface -> a -> C a
initWith i v =
    C i (Leaf v)


before : C a -> C a -> C a
before second first =
    case ( first, second ) of
        ( Unit, _ ) ->
            second

        ( _, Unit ) ->
            first

        ( C il _, C ir _ ) ->
            C (il |> I.before ir) (Sequenced first second)


aside : C a -> C a -> C a
aside second first =
    case ( first, second ) of
        ( Unit, _ ) ->
            second

        ( _, Unit ) ->
            first

        ( C il _, C ir _ ) ->
            C (il |> I.aside ir) (Aside first second)


interface : C a -> Interface
interface c =
    case c of
        Unit ->
            I.unit

        C i _ ->
            i
