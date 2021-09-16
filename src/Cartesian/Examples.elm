module Cartesian.Examples exposing (basicCell, a, b, c, d, abc, axb, bxa, axb_cxd, axb_cxd_e, simpleBypass, bypass, logo)

{-| Example Cartesian diagrams with String labels

# ![elm-wiring-diagrams](https://github.com/jcberentsen/elm-wiring-diagrams/blob/main/assets/png/bypass.png?raw=true)

## Usage

@docs basicCell, a, b, c, d, abc, axb, bxa, axb_cxd, axb_cxd_e, simpleBypass, bypass, logo

-}

import Cartesian as C exposing (C)


{-| A basic cell with a label String of 'Cell'
-}
basicCell : C String
basicCell =
    C.init "Cell"


{-| A cell with an 'a'

    a =
        C.init "a"

-}
a : C String
a =
    C.init "a"


{-| A cell with a 'b'
-}
b : C String
b =
    C.init "b"


{-| A cell with a 'c'
-}
c : C String
c =
    C.init "c"


{-| Wow a cell with a 'd'
-}
d : C String
d =
    C.init "d"


{-| We can compose a b and c in sequence
Ah, now it is starting to make sense...

    a |> C.before b |> C.before c

-}
abc : C String
abc =
    a |> C.before b |> C.before c


{-| Compose a and b in parallel

    a |> C.aside b

-}
axb : C String
axb =
    a |> C.aside b


{-| Compose a and b in parallel but b first
-}
bxa : C String
bxa =
    b |> C.aside a


{-| Compose axb and cxd in sequence.
We can expect a to connect with c and b to connect with d

    let
        cxd =
            c
                |> C.aside d
    in
    axb |> C.before cxd

-}
axb_cxd : C String
axb_cxd =
    let
        cxd =
            c
                |> C.aside d
    in
    axb |> C.before cxd


{-| Compose axb and cxd in sequence `before` 'e'
The 'e' node needs to be declared with 2 inputs so the outputs from both 'c' and 'd'
will connect to it. Otherwise the 'd' output will be dangling (which is ok if something else will be connected later)
-}
axb_cxd_e : C String
axb_cxd_e =
    axb_cxd |> C.before (C.initWith 2 1 "e")


{-| A simple bypass

    let
        source =
            C.initWith 1 3 "src"

        sink =
            C.initWith 1 1 "sink"
    in
    source |> C.before (sink |> C.aside bxa)

-}
simpleBypass : C String
simpleBypass =
    let
        source =
            C.initWith 1 3 "src"

        sink =
            C.initWith 1 1 "sink"
    in
    source |> C.before (sink |> C.aside bxa)


{-| An example of how to make a bypass

    let
        source3 =
            C.initWith 1 3 "src"

        sink2 =
            C.initWith 2 1 "sink"

        extraLane =
            C.init "bypass"

        conduce =
            axb_cxd_e |> C.aside extraLane
    in
    source3 |> C.before conduce |> C.before sink2

-}
bypass : C String
bypass =
    let
        source3 =
            C.initWith 1 3 "src"

        sink2 =
            C.initWith 2 1 "sink"

        extraLane =
            C.init "bypass"

        conduce =
            axb_cxd_e |> C.aside extraLane
    in
    source3 |> C.before conduce |> C.before sink2


{-| The diagram for the package logo

    C.init "elm"
        |> C.before (C.init "wiring")
        |> C.before (C.init "diagrams")

-}
logo : C String
logo =
    C.init "elm"
        |> C.before (C.init "wiring")
        |> C.before (C.init "diagrams")
