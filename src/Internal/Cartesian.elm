module Internal.Cartesian exposing
    ( C(..)
    , Composed(..)
    , aside
    , before
    , group
    , init
    , initWith
    , interface
    , map
    , unit
    , wrap
    )

import Internal.Cartesian.Interface as I exposing (Interface)


type C a
    = Unit
    | C Interface (Composed a)


type Composed a
    = Leaf a
    | Aside (C a) (C a)
    | Sequenced (C a) (C a)
    | Wrap a (C a)


map : (a -> b) -> C a -> C b
map f c =
    case c of
        Unit ->
            Unit

        C i cs ->
            C i <|
                case cs of
                    Leaf a ->
                        Leaf (f a)

                    Aside a b ->
                        Aside (map f a) (map f b)

                    Sequenced a b ->
                        Sequenced (map f a) (map f b)

                    Wrap a b ->
                        Wrap (f a) (map f b)


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


wrap : a -> C a -> C a
wrap label a =
    case a of
        Unit ->
            Unit

        C i _ ->
            C i (Wrap label a)


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
