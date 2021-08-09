module WiringDiagram exposing
    ( Diagram
    , init
    , initLabeled, diagram
    , source, sink, relation, inSequence, inParallel, setLabel
    , initWrap
    )

{-| Wiring diagrams


# Inspiration

<https://arxiv.org/pdf/2101.12046.pdf>


# Diagram type

@docs Diagram


# Simple use

See ['WiringDiagram.Simple'](@WiringDiagram.Simple)

@docs init


# Custom use

@docs initLabeled, diagram


# Diagram helpers

@docs source, sink, relation, inSequence, inParallel, setLabel


# Wrapping

Parts of a Diagram can be wrapped and given a label of its own (with setLabel)

@docs initWrap

-}

import Internal.WiringDiagram as I


{-| Represent an abstract wire-diagram.

Think of boxes connected with ports, that can be inside other boxes (which connect outside)
(TODO illustration)

-}
type alias Diagram a =
    I.Diagram a


{-| Initialize an empty Diagram. The idea is a box with no label.

The box is abstract and will be given geometry using [the Layout module](@Layout)

Use modifiers like `setInPorts` to configure further

-}
init : Diagram a
init =
    I.init


{-| Initialize an empty Diagram with a label of a chosen type

All diagrams you intend to combine needs to have the same label type

-}
initLabeled : Maybe a -> Diagram a
initLabeled =
    I.initLabeled


{-| Wrap a diagram, exposing the free in and out ports
-}
diagram : { a | label : Maybe b, inner : List (Diagram b) } -> Diagram b
diagram =
    I.diagram


{-| Make a source with label a and a number of outPorts
-}
source : a -> Int -> Diagram a
source =
    I.source


{-| Make a sink with label a and a number of inPorts
-}
sink : a -> Int -> Diagram a
sink =
    I.sink


{-| Make a relation with label a and a number of inPorts and outPorts
-}
relation : a -> Int -> Int -> Diagram a
relation =
    I.relation


{-| Connect diagrams in a chain
-}
inSequence : List (Diagram a) -> Diagram a
inSequence =
    I.inSequence


{-| Put diagrams in parallel.

No connections will be made.

(We probably need to relabel ports here...)

-}
inParallel : List (Diagram a) -> Diagram a
inParallel =
    I.inParallel


{-| Wrap a diagram, exposing the free in and out ports
-}
initWrap : Diagram a -> Diagram a
initWrap =
    I.initWrap


{-| Assign a label to a Diagram

    setLabel (Just "node") <| diagram --

-}
setLabel : Maybe a -> Diagram a -> Diagram a
setLabel =
    I.setLabel
