module WiringDiagram.Examples exposing (sampleDiagrams, a, b, c)

{-| Some example diagrams

@docs sampleDiagrams, a, b, c

-}

import WiringDiagram as D exposing (..)


type alias Diagram =
    D.Diagram String


{-| A list of some diagrams

    sampleDiagrams : List Diagram
    sampleDiagrams =
        [ inSequence [ a, b, c ]
        , inSequence [ wrap a, wrap b ]
        , inSequence [ wrap a, wrap (inParallel [ b, c |> offsetPorts 1 ]) ]
        , wrap_a
        , inSequence
            [ wrap (source "a" 2)
            , sink "b" 2
            ]
        , wrap_axb_c
        , inSequence [ a, b ]
        , inSequence [ source "a3" 3, sink "b2" 2 ]
        ]

-}
sampleDiagrams : List Diagram
sampleDiagrams =
    [ inSequence [ a, b, c ]
    , inSequence [ wrap a, wrap b ]
    , inSequence [ wrap a, wrap (inParallel [ b, c |> offsetPorts 1 ]) ]
    , wrap_a
    , inSequence
        [ wrap (source "a" 2)
        , sink "b" 2
        ]
    , wrap_axb_c
    , inSequence [ a, b ]
    , inSequence [ source "a3" 3, sink "b2" 2 ]
    ]


wrap : Diagram -> Diagram
wrap =
    D.initWrap


wrap_a : Diagram
wrap_a =
    wrap a


wrap_axb_c : Diagram
wrap_axb_c =
    inSequence
        [ wrap <|
            inParallel
                [ source "a" 1
                , source "b" 1 |> offsetPorts 1
                ]
        , sink "c" 2
        ]


{-| A simple source labeled 'a'
It has one output port.
-}
a : Diagram
a =
    source "a" 1


{-| A simple relation labeled 'b'
It has one input and one output port
-}
b : Diagram
b =
    relation "b" 1 1


{-| A simple sink labeled 'c'
It has one input port
-}
c : Diagram
c =
    sink "c" 1
