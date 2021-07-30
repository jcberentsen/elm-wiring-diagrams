module WiringDiagram.Examples exposing (sampleDiagrams)

import WiringDiagram
    exposing
        ( inParalell
        , inSequence
        , offsetPorts
        , relation
        , sink
        , source
        )
import WiringDiagram.Simple exposing (..)



-- Wiring diagrams
-- https://arxiv.org/pdf/2101.12046.pdf


sampleDiagrams : List Diagram
sampleDiagrams =
    [ inSequence [ a, b, c ]
    , inSequence [ wrap a, wrap b ]
    , inSequence [ wrap a, wrap (inParalell [ b, c |> offsetPorts 1 ]) ]
    , wrap_a
    , inSequence
        [ wrap (source "a" 2)
        , sink "b" 2
        ]
    , wrap_axb_c
    , inSequence [ a, b ]
    , inSequence [ source "a3" 3, sink "b2" 2 ]
    ]


wrap_a : Diagram
wrap_a =
    wrap a


wrap_axb_c : Diagram
wrap_axb_c =
    inSequence
        [ wrap <|
            inParalell
                [ source "a" 1
                , source "b" 1 |> offsetPorts 1
                ]
        , sink "c" 2
        ]


a : Diagram
a =
    source "a" 1


b : Diagram
b =
    relation "b" 1 1


c : Diagram
c =
    sink "c" 1
