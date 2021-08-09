module Internal.WiringDiagram.Layout exposing
    ( Layout(..)
    , layoutDiagram
    , layoutDiagramWithConfig
    , Polarity(..), Transform, arrowExtent, portPositionsOfBox
    , bound, chain, chainLayouts, empty, par, parLayouts, shift
    )

{-| Layout of WiringDiagram values

Typically a Layout can be converted to SVG for display


# Definition

@docs Layout


# Simple Layout of a Diagram

@docs layoutDiagram


# Customized Layout of a Diagram

@docs layoutDiagramWithConfig


# Misc

@docs Polarity, Pos, Transform, arrowExtent, portPositionsOfBox

-}

import Dict
import Internal.Bound as Bound exposing (Bound, hull)
import Internal.Extent as Extent
import Internal.WiringDiagram exposing (..)
import WiringDiagram.Layout.Box as Box exposing (Box)
import WiringDiagram.Layout.Config as Config exposing (Config)
import WiringDiagram.Vec2 as Vec exposing (..)


{-| The Layout type keeps information about the layout of a Wiring diagram.
-}
type Layout a
    = Group
        { transform : Transform
        , exterior : Maybe (Box a)
        , interiorTransform : Transform
        , interior : List (Layout a)
        }
    | Item (Box a)
    | Arrow
        { label : String
        , tail : Vec2
        , head : Vec2
        }


empty : Layout a
empty =
    Group
        { transform = Vec2 0 0
        , exterior = Nothing
        , interiorTransform = Vec2 0 0
        , interior = []
        }


{-| Transform only supports simple 2d translation
-}
type alias Transform =
    Vec2


{-| Layout a Diagram with default configuration
-}
layoutDiagram : Diagram a -> Layout a
layoutDiagram =
    layoutDiagramWithConfig <| Config.init (\_ _ -> "arrow") (Vec2 40 20)


{-| Layout a Diagram with custom configuration
-}
layoutDiagramWithConfig : Config a -> Diagram a -> Layout a
layoutDiagramWithConfig config d =
    let
        (Diagram dia) =
            d

        innerLayout =
            if List.isEmpty dia.inner then
                let
                    box =
                        Box.boxify d
                in
                Item box

            else
                splay config dia.direction dia.inner
    in
    if dia.wrap then
        let
            wrapping =
                setInPorts dia.inPorts <|
                    setOutPorts dia.outPorts <|
                        grow 10 <|
                            Maybe.withDefault (Box.boxify d) <|
                                Maybe.map (Box.fromExtent dia.label) <|
                                    bound innerLayout

            -- we need to compute the arrow across the interiorTransform, so
            -- we revert it temporarily in the wrapping
            reverseWrapping =
                { wrapping
                    | lo = Vec.translate (Vec2 -10 -10) wrapping.lo
                }

            arrows =
                computeArrowsDir Out Out innerLayout (Item reverseWrapping)
                    ++ computeArrowsDir In In (Item reverseWrapping) innerLayout
        in
        Group
            { transform = Vec2 0 0
            , exterior = Just wrapping
            , interiorTransform = Vec2 10 10 --^ padding
            , interior = arrows ++ [ innerLayout ]
            }

    else
        innerLayout


splay : Config a -> Direction -> List (Diagram a) -> Layout a
splay config dir inner =
    case dir of
        D1 ->
            chain config inner

        D2 ->
            par config inner


{-| Compose diagrams in sequence
-}
chain : Config a -> List (Diagram a) -> Layout a
chain config ds =
    let
        subLayouts =
            List.map (layoutDiagramWithConfig config) ds
    in
    chainLayouts config subLayouts


chainLayouts : Config a -> List (Layout a) -> Layout a
chainLayouts config subLayouts =
    let
        maxHeight =
            Maybe.withDefault 0 <| List.maximum <| List.map heightOf subLayouts

        horizontallyCentered sub ( x, acc ) =
            let
                tr =
                    Vec2 x ((maxHeight - heightOf sub) / 2)

                positioned =
                    shift tr sub

                arrows =
                    case List.head <| List.reverse acc of
                        Just l ->
                            computeArrows l positioned

                        _ ->
                            []
            in
            ( x + widthOf sub + (Config.spacing config).x
            , acc ++ arrows ++ [ positioned ]
            )
    in
    Group
        { transform = Vec2 0 0
        , exterior = Nothing
        , interiorTransform = Vec2 0 0
        , interior = Tuple.second <| List.foldl horizontallyCentered ( 0, [] ) subLayouts
        }


{-| Compute arrows between two layouts.
Ports going Out of the first layout will be connected to matching ports going In to the second layout
-}
computeArrows : Layout msg -> Layout msg -> List (Layout msg)
computeArrows =
    computeArrowsDir Out In


{-| Compute arrows between two layouts given some directions.
-}
computeArrowsDir : Polarity -> Polarity -> Layout a -> Layout b -> List (Layout c)
computeArrowsDir fromDir toDir layoutA layoutB =
    let
        from =
            Dict.fromList <| portPositionsOfLayout fromDir layoutA

        to =
            Dict.fromList <| portPositionsOfLayout toDir layoutB

        drop _ _ r =
            r

        arrow _ a b r =
            Arrow
                { label = "arrow"
                , tail = a
                , head = b
                }
                :: r
    in
    Dict.merge drop arrow drop from to []


{-| compose parallell (independent lanes)
-}
par : Config a -> List (Diagram a) -> Layout a
par config ds =
    let
        subLayouts =
            List.map (layoutDiagramWithConfig config) ds
    in
    parLayouts config subLayouts


parLayouts : Config a -> List (Layout a) -> Layout a
parLayouts config subLayouts =
    let
        vertically : Layout a -> ( Float, List (Layout a) ) -> ( Float, List (Layout a) )
        vertically item ( y, acc ) =
            let
                spacing =
                    Config.spacing config

                sub =
                    let
                        ty =
                            Vec2 0 y
                    in
                    shift ty item
            in
            ( y + heightOf sub + spacing.y
            , acc ++ [ sub ]
            )
    in
    Group
        { transform = Vec2 0 0
        , exterior = Nothing
        , interiorTransform = Vec2 0 0
        , interior = Tuple.second <| List.foldl vertically ( 0, [] ) subLayouts
        }


bound : Layout a -> Bound
bound l =
    case l of
        Item b ->
            Just (Box.toExtent b)

        Group g ->
            let
                exteriorBound =
                    case g.exterior of
                        Just b ->
                            Bound.init <| Extent.translate g.transform <| Box.toExtent b

                        _ ->
                            Bound.empty
            in
            hull <|
                exteriorBound
                    :: List.map
                        (Maybe.map (Extent.translate g.transform << Extent.translate g.interiorTransform)
                            << bound
                        )
                        g.interior

        Arrow arr ->
            Just (arrowExtent arr)


{-| Make an extent for an Arrow. This stupidly assumes the tail is < head
-}
arrowExtent : { a | tail : b, head : c } -> { lo : b, hi : c }
arrowExtent arr =
    { lo = arr.tail
    , hi = arr.head
    }



-- toExtent : Box a -> Extent
-- toExtent b =
--     { lo = b.lo
--     , hi = { x = b.lo.x + b.width, y = b.lo.y + b.height }
--     }
--
-- transformExtent : Vec2 -> Extent -> Extent
-- transformExtent tx e =
--     { lo = translate tx e.lo
--     , hi = translate tx e.hi
--     }
-- Hm, not too good. Is extent a monoid?
-- Perhaps interval algebra is better here?
-- nullExtent : { lo : { x : number, y : number }, hi : { x : number, y : number } }
-- nullExtent =
--     { lo = { x = 0, y = 0 }
--     , hi = { x = 0, y = 0 }
--     }
-- combineExtents : List Extent -> Extent
-- combineExtents ls =
--     let
--         combine b acc =
--             let
--                 lo =
--                     { x = min b.lo.x acc.lo.x
--                     , y = min b.lo.y acc.lo.y
--                     }
--                 hi =
--                     { x = max b.hi.x acc.hi.x
--                     , y = max b.hi.y acc.hi.y
--                     }
--             in
--             { acc | lo = lo, hi = hi }
--     in
--     List.foldr combine nullExtent ls


shift : Transform -> Layout a -> Layout a
shift t l =
    case l of
        Item b ->
            Item
                { b
                    | lo = Vec.translate t b.lo
                }

        Group g ->
            Group { g | transform = Vec.translate t g.transform }

        Arrow arr ->
            Arrow
                { arr
                    | tail = Vec.translate t arr.tail
                    , head = Vec.translate t arr.head
                }


widthOf : Layout a -> Float
widthOf l =
    case bound l of
        Just b ->
            b.hi.x - b.lo.x

        _ ->
            0


heightOf : Layout a -> Float
heightOf l =
    case bound l of
        Just b ->
            b.hi.y - b.lo.y

        _ ->
            0


{-| Polarity of a Port. Is the port going In or Out of something
-}
type Polarity
    = In
    | Out


{-| Grow the width and height of a Box. Does not move the local origin.
-}
grow : Float -> Box a -> Box a
grow v b =
    { b
        | height = b.height + 2 * v
        , width = b.width + 2 * v
    }


{-| List the port positions of a Box, given a Polarity (In,Out)
-}
portPositionsOfBox : Polarity -> Box a -> List ( Int, Vec2 )
portPositionsOfBox d b =
    let
        ports =
            case d of
                In ->
                    b.inPorts

                Out ->
                    b.outPorts

        range =
            toFloat <| 2 + (List.maximum ports |> Maybe.withDefault 0)

        convert i =
            ( i, boxPortPosN range i d b )
    in
    List.map convert ports


{-| List the port positions of a Layout, given a Polarity (In,Out)
-}
portPositionsOfLayout : Polarity -> Layout a -> List ( Int, Vec2 )
portPositionsOfLayout d l =
    case l of
        Item a ->
            portPositionsOfBox d a

        Group g ->
            let
                offset ( i, p ) =
                    ( i, Vec.translate p g.transform )
            in
            case g.exterior of
                Just outer ->
                    List.map offset <| portPositionsOfBox d outer

                _ ->
                    List.concatMap (List.map offset << portPositionsOfLayout d) g.interior

        _ ->
            []


boxPortPosN : Float -> Int -> Polarity -> Box a -> Vec2
boxPortPosN range i d a =
    let
        edgeX =
            case d of
                In ->
                    a.lo.x

                Out ->
                    a.lo.x + a.width
    in
    { x = edgeX
    , y = a.lo.y + ((toFloat i + 1) * a.height / range)
    }
