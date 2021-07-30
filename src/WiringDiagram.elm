module WiringDiagram exposing
    ( Diagram(..)
    , Direction(..)
    , Ports
    , dia
    , diagram
    , inParalell
    , inPorts
    , inSequence
    , include
    , init
    , initLabeled
    , initWrap
    , lastAsList
    , map
    , offsetPorts
    , outPorts
    , relation
    , setDirection
    , setInPorts
    , setOutPorts
    , sink
    , source
    )

{-| Wiring diagrams


# Inspiration

<https://arxiv.org/pdf/2101.12046.pdf>

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


dia : Diagram a
dia =
    init


init : Diagram a
init =
    initLabeled Nothing


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


offsetPorts : Int -> Diagram a -> Diagram a
offsetPorts dp (Diagram d) =
    Diagram
        { d
            | inPorts = List.map ((+) dp) d.inPorts
            , outPorts = List.map ((+) dp) d.outPorts
        }


type Direction
    = D1
    | D2


type alias Ports =
    List Int


inPorts : Diagram a -> Ports
inPorts (Diagram d) =
    d.inPorts


outPorts : Diagram a -> Ports
outPorts (Diagram d) =
    d.outPorts


include : List (Diagram a) -> Diagram a -> Diagram a
include ds (Diagram d) =
    Diagram { d | inner = ds }



--| wrap a diagram, exposing the free in and out ports


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


source : a -> Int -> Diagram a
source label portCount =
    initLabeled (Just label) |> map (setOutPorts (List.range 0 (portCount - 1)))


sink : a -> Int -> Diagram a
sink label portCount =
    initLabeled (Just label) |> map (setInPorts (List.range 0 (portCount - 1)))


relation : a -> Int -> Int -> Diagram a
relation label inPortCount outPortCount =
    initLabeled (Just label)
        |> map (setInPorts (List.range 0 (inPortCount - 1)))
        |> map (setOutPorts (List.range 0 (outPortCount - 1)))


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



-- We probably need to relabel ports here...


inParalell : List (Diagram a) -> Diagram a
inParalell items =
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


setInPorts :
    List Int
    -> { a | inPorts : Ports }
    -> { a | inPorts : Ports }
setInPorts ps d =
    { d | inPorts = ps }


setOutPorts :
    List Int
    -> { a | outPorts : Ports }
    -> { a | outPorts : Ports }
setOutPorts ps d =
    { d | outPorts = ps }


setWrap : a -> { b | wrap : a } -> { b | wrap : a }
setWrap d w =
    { w | wrap = d }


setDirection :
    Direction
    -> { a | direction : Direction }
    -> { a | direction : Direction }
setDirection dir d =
    { d | direction = dir }
