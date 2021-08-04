module WiringDiagram exposing
    ( Diagram(..)
    , init
    , initLabeled, diagram
    , source, sink, relation, inSequence, inParallel, setLabel, setDirection, Direction(..), include
    , setOutPorts, setInPorts, outPorts, inPorts
    , initWrap, setWrap
    , map, Ports, offsetPorts
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

@docs source, sink, relation, inSequence, inParallel, setLabel, setDirection, Direction, include


# Input and output Ports

@docs setOutPorts, setInPorts, outPorts, inPorts


# Wrapping

Parts of a Diagram can be wrapped and given a label of its own (with setLabel)

@docs initWrap, setWrap


# Misc

@docs map, Ports, offsetPorts

-}


{-| Represent an abstract wire-diagram.

Think of boxes connected with ports, that can be inside other boxes (which connect outside)
(TODO illustration)

-}
type Diagram a
    = Diagram
        { label : Maybe a
        , wrap : Bool
        , inner : List (Diagram a)
        , direction : Direction
        , inPorts : Ports
        , outPorts : Ports
        }


{-| Initialize an empty Diagram. The idea is a box with no label.
The box is abstract and will be given geometry using [the Layout module](@Layout)

Use modifiers like `setInPorts` to configure further

-}
init : Diagram a
init =
    initLabeled Nothing


{-| Initialize an empty Diagram with a label of a chosen type

All diagrams you intend to combine needs to have the same label type

-}
initLabeled : Maybe a -> Diagram a
initLabeled m =
    Diagram
        { label = m
        , wrap = False
        , inner = []
        , direction = D1
        , inPorts = []
        , outPorts = []
        }


{-| Wrap a diagram, exposing the free in and out ports
-}
diagram : { a | label : Maybe b, inner : List (Diagram b) } -> Diagram b
diagram schema =
    Diagram
        { label = schema.label
        , wrap = False
        , inner = schema.inner
        , direction = D1
        , inPorts = []
        , outPorts = []
        }


{-| Make a source with label a and a number of outPorts
-}
source : a -> Int -> Diagram a
source label portCount =
    initLabeled (Just label) |> map (setOutPorts (List.range 0 (portCount - 1)))


{-| Make a sink with label a and a number of inPorts
-}
sink : a -> Int -> Diagram a
sink label portCount =
    initLabeled (Just label) |> map (setInPorts (List.range 0 (portCount - 1)))


{-| Make a relation with label a and a number of inPorts and outPorts
-}
relation : a -> Int -> Int -> Diagram a
relation label inPortCount outPortCount =
    initLabeled (Just label)
        |> map (setInPorts (List.range 0 (inPortCount - 1)))
        |> map (setOutPorts (List.range 0 (outPortCount - 1)))


{-| Connect diagrams in a chain
-}
inSequence : List (Diagram a) -> Diagram a
inSequence items =
    let
        ins =
            List.concatMap inPorts <| List.take 1 items

        out =
            List.concatMap outPorts <| lastAsList items
    in
    init
        |> include items
        |> map (setOutPorts out >> setInPorts ins)


{-| Put diagrams in paralell. No connections will be made.
(We probably need to relabel ports here...)
-}
inParallel : List (Diagram a) -> Diagram a
inParallel items =
    let
        ins =
            List.concatMap inPorts <| items

        out =
            List.concatMap outPorts <| items
    in
    init
        |> include items
        |> map (setDirection D2)
        |> map (setOutPorts out >> setInPorts ins)


{-| Bump the port numbers of a diagram.

This is used to avoid duplicate ports when combining diagrams in paralell
(relabel ports here...)

-}
offsetPorts : Int -> Diagram a -> Diagram a
offsetPorts dp (Diagram d) =
    Diagram
        { d
            | inPorts = List.map ((+) dp) d.inPorts
            , outPorts = List.map ((+) dp) d.outPorts
        }


{-| Direction of composition of diagrams

D1 is horizontal (sequenced, chained)
D2 is vertical (in paralell)

-}
type Direction
    = D1
    | D2


{-| Ports are a list of numbers identifying each port
-}
type alias Ports =
    List Int


{-| Extract the input ports of a Diagram
-}
inPorts : Diagram a -> Ports
inPorts (Diagram d) =
    d.inPorts


{-| Extract the output ports of a Diagram
-}
outPorts : Diagram a -> Ports
outPorts (Diagram d) =
    d.outPorts


{-| Include a list of diagrams inside another diagram.
The included list will replace any previous innards.
-}
include : List (Diagram a) -> Diagram a -> Diagram a
include ds (Diagram d) =
    Diagram { d | inner = ds }


{-| Wrap a diagram, exposing the free in and out ports
-}
initWrap : Diagram a -> Diagram a
initWrap di =
    let
        (Diagram d) =
            di
    in
    init |> include [ di ] |> map (setWrap True >> setOutPorts d.outPorts >> setInPorts d.inPorts)


lastAsList : List a -> List a
lastAsList l =
    List.drop (List.length l - 1) l


{-| Map a function over a diagram innards
You should avoid using this
-}
map :
    ({ label : Maybe a
     , wrap : Bool
     , inner : List (Diagram a)
     , direction : Direction
     , inPorts : Ports
     , outPorts : Ports
     }
     ->
        { label : Maybe b
        , wrap : Bool
        , inner : List (Diagram b)
        , direction : Direction
        , inPorts : Ports
        , outPorts : Ports
        }
    )
    -> Diagram a
    -> Diagram b
map f (Diagram d) =
    Diagram <| f d


{-| Assign input ports to a Diagram
-}
setInPorts :
    List Int
    -> { a | inPorts : Ports }
    -> { a | inPorts : Ports }
setInPorts ps d =
    { d | inPorts = ps }


{-| Assign output ports to a Diagram
-}
setOutPorts :
    List Int
    -> { a | outPorts : Ports }
    -> { a | outPorts : Ports }
setOutPorts ps d =
    { d | outPorts = ps }


{-| Specify that a diagram exterior is significant
-}
setWrap : a -> { b | wrap : a } -> { b | wrap : a }
setWrap d w =
    { w | wrap = d }


{-| Say which direction the children should lay out
-}
setDirection :
    Direction
    -> { a | direction : Direction }
    -> { a | direction : Direction }
setDirection dir d =
    { d | direction = dir }


{-| Assign a label to a Diagram

    setLabel (Just "node") <| diagram --

-}
setLabel : Maybe a -> Diagram a -> Diagram a
setLabel ma (Diagram d) =
    Diagram { d | label = ma }
