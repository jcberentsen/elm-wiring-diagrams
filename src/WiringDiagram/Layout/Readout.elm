module WiringDiagram.Layout.Readout exposing
    ( findExtent
    , pickPair
    , traverseArrows
    , traverseOuterBoxes
    )

import WiringDiagram.Layout exposing (..)
import WiringDiagram.Layout.Box exposing (..)
import WiringDiagram.Layout.Extent as Extent exposing (Extent)
import WiringDiagram.Vec2 as Vec exposing (..)


pickPair : ( a, String ) -> ( a, String ) -> Layout a -> Maybe ( Extent, Extent )
pickPair labelA labelB l =
    let
        a =
            findExtent labelA l

        b =
            findExtent labelB l
    in
    case ( a, b ) of
        ( Just foundA, Just foundB ) ->
            Just ( foundA, foundB )

        _ ->
            Nothing



--| This folds the transformations


findExtent : ( a, String ) -> Layout a -> Maybe Extent
findExtent needle layout =
    let
        ( label, labelString ) =
            needle
    in
    case layout of
        Item b ->
            if b.label == Just label then
                Just (toExtent b)

            else
                Nothing

        Arrow a ->
            if a.label == labelString then
                Just <| arrowExtent a

            else
                Nothing

        Group g ->
            case g.exterior of
                Just b ->
                    if b.label == Just label then
                        Maybe.map (Extent.map (Vec.translate g.transform)) <|
                            Just (toExtent b)

                    else
                        Maybe.map (Extent.map (Vec.translate g.transform << Vec.translate g.interiorTransform)) <|
                            List.head <|
                                List.filterMap (findExtent needle) g.interior

                Nothing ->
                    Maybe.map (Extent.map (Vec.translate g.transform << Vec.translate g.interiorTransform)) <|
                        List.head <|
                            List.filterMap (findExtent needle) g.interior


traverseArrows : ({ label : String, tail : Vec2, head : Vec2 } -> a) -> Layout b -> List a
traverseArrows f layout =
    case layout of
        Arrow a ->
            [ f a ]

        Group g ->
            List.concatMap (traverseArrows f) g.interior

        Item _ ->
            []


traverseOuterBoxes : (List (Box a) -> List (Box a)) -> Layout a -> List (Box a)
traverseOuterBoxes f layout =
    let
        itemBox l =
            case l of
                Item b ->
                    [ b ]

                Group g ->
                    case g.exterior of
                        Just b ->
                            [ b ]

                        _ ->
                            -- stop
                            []

                _ ->
                    []
    in
    case layout of
        Arrow _ ->
            []

        Group g ->
            case g.exterior of
                Just b ->
                    [ b ]

                _ ->
                    List.concatMap (f << itemBox) g.interior

        Item _ ->
            []
