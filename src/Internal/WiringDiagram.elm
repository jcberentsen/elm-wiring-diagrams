module Internal.WiringDiagram exposing
    ( Diagram(..)
    , Direction(..)
    , Ports
    , alignPorts
    , diagram
    , inParallel
    , inPorts
    , inSequence
    , include
    , init
    , initLabeled
    , initWrap
    , map
    , offsetPorts
    , outPorts
    , relation
    , setDirection
    , setInPorts
    , setLabel
    , setOutPorts
    , setWrap
    , sink
    , source
    )


type Diagram a
    = Diagram
        { label : Maybe a
        , wrap : Bool
        , inner : List (Diagram a)
        , direction : Direction
        , inPorts : Ports
        , outPorts : Ports
        }


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


{-| Put diagrams in parallel. No connections will be made.
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


{-| Align ports when composing in the given direction

In the D1 direction (sequential):

  - port numbers will be bumped to match the output ports of a preceding element
  - The diagrams must match on input and output cardinality. The function returns Nothing if they don't

In the D2 direction (parallel): port numbers will be bumped to not overlap ports of a preceding element

-}
alignPorts : Direction -> List (Diagram a) -> Maybe (List (Diagram a))
alignPorts direction diagrams =
    case direction of
        D1 ->
            case diagrams of
                head :: rest ->
                    let
                        initial : PortWalker a
                        initial =
                            { previousOut = outPorts head
                            , result = Just [ head ]
                            }

                        step :
                            Diagram a
                            -> PortWalker a
                            -> PortWalker a
                        step r pw =
                            let
                                cardinalityIn =
                                    List.length (inPorts r)
                            in
                            { previousOut = outPorts r
                            , result =
                                if List.length pw.previousOut == cardinalityIn then
                                    let
                                        offset =
                                            (List.minimum pw.previousOut |> Maybe.withDefault 0)
                                                - (List.minimum (outPorts r) |> Maybe.withDefault 0)
                                    in
                                    Maybe.map (\l -> offsetPorts offset r :: l) pw.result

                                else
                                    let
                                        interfacePorts =
                                            pw.previousOut
                                    in
                                    Maybe.map (\l -> map (setInPorts interfacePorts) r :: l) pw.result
                            }
                    in
                    Maybe.map List.reverse <| .result <| List.foldl step initial rest

                _ ->
                    Just []

        D2 ->
            case diagrams of
                head :: rest ->
                    let
                        initial : PortWalkerD2 a
                        initial =
                            { previousIn = inPorts head
                            , previousOut = outPorts head
                            , result = Just [ head ]
                            }

                        step :
                            Diagram a
                            -> PortWalkerD2 a
                            -> PortWalkerD2 a
                        step r pw =
                            let
                                offset =
                                    -- TODO this is naive
                                    List.maximum pw.previousOut |> Maybe.withDefault 0 |> (+) 1

                                updated =
                                    offsetPorts offset r
                            in
                            { previousIn = inPorts r
                            , previousOut = outPorts r
                            , result = Maybe.map (\l -> updated :: l) pw.result
                            }
                    in
                    Maybe.map List.reverse <| .result <| List.foldl step initial rest

                _ ->
                    Just []


type alias PortWalker a =
    { previousOut : Ports
    , result : Maybe (List (Diagram a))
    }


type alias PortWalkerD2 a =
    { previousIn : Ports
    , previousOut : Ports
    , result : Maybe (List (Diagram a))
    }


{-| Bump the port numbers of a diagram.

This is used to avoid duplicate ports when combining diagrams in parallel
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
D2 is vertical (in parallel)

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
