module Internal.Cartesian.Interface exposing (Interface(..), aside, before, incompatibility, init, unit)


type Interface
    = Unital
    | Arity Hom
    | Incompatibility Hom


unit : Interface
unit =
    Unital


type alias Hom =
    { inp : Int
    , out : Int
    }


init : Int -> Int -> Interface
init inputArity outputArity =
    Arity { inp = inputArity, out = outputArity }


incompatibility : Int -> Int -> Interface
incompatibility o i =
    Incompatibility { inp = i, out = o }


{-| Note the arguments are in flipped order to support

    a |> before b

meaning a will be before b

-}
before : Interface -> Interface -> Interface
before second first =
    case ( first, second ) of
        ( Unital, _ ) ->
            second

        ( _, Unital ) ->
            first

        ( Arity l, Arity r ) ->
            if l.out == r.inp then
                Arity { inp = l.inp, out = r.out }

            else
                Incompatibility { out = l.out, inp = r.inp }

        ( Incompatibility _, _ ) ->
            first

        ( _, Incompatibility _ ) ->
            second


{-| Note the arguments are in flipped order to support

    a |> aside b

meaning a will be 'above' b

-}
aside : Interface -> Interface -> Interface
aside second first =
    case ( first, second ) of
        ( Unital, _ ) ->
            second

        ( _, Unital ) ->
            first

        ( Arity l, Arity r ) ->
            Arity
                { inp = l.inp + r.inp
                , out = l.out + r.out
                }

        ( Incompatibility _, _ ) ->
            first

        ( _, Incompatibility _ ) ->
            second
