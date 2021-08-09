module Cartesian.Layout exposing
    ( toLayout
    , toLayoutWithConfig
    )

{-| Layout a cartesian structure


## Usage

@docs toLayout

-}

import Cartesian.Internal as C exposing (C(..))
import WiringDiagram.Layout.Box as Box
import WiringDiagram.Layout.Config as Config exposing (Config)
import WiringDiagram.Layout.Extent exposing (computeCenterY)
import WiringDiagram.Layout.Internal as L exposing (Layout)
import WiringDiagram.Vec2 exposing (Vec2)


{-| Layout a cartesian structure

This is unfinished, and won't do anything interesting yet.

-}
toLayout : C a -> Layout a
toLayout =
    toLayoutWithConfig <| Config.init (\_ _ -> "arrow") (Vec2 40 20)


arrowHeadLength : number
arrowHeadLength =
    10


toLayoutWithConfig : Config a -> C a -> Layout a
toLayoutWithConfig config c =
    case c of
        C.Unit ->
            L.empty

        C interface composition ->
            let
                inner =
                    composeLayout config composition

                innerExtent =
                    L.bound inner

                ( incoming, outgoing ) =
                    case interface of
                        _ ->
                            -- I.Unital ->
                            let
                                i =
                                    L.Arrow
                                        { label = "arrow"
                                        , tail = { x = innerExtent.lo.x, y = computeCenterY innerExtent }
                                        , head = { x = innerExtent.lo.x + arrowHeadLength, y = computeCenterY innerExtent }
                                        }

                                o =
                                    L.Arrow
                                        { label = "arrow"
                                        , tail =
                                            { x = innerExtent.hi.x + arrowHeadLength
                                            , y = computeCenterY innerExtent
                                            }
                                        , head =
                                            { x = innerExtent.hi.x + 2 * arrowHeadLength
                                            , y = computeCenterY innerExtent
                                            }
                                        }
                            in
                            ( i, o )
            in
            L.Group
                { transform = Vec2 0 0
                , exterior = Nothing
                , interiorTransform = Vec2 0 0
                , interior = [ incoming, L.shift { x = arrowHeadLength, y = 0 } inner, outgoing ]

                --Tuple.second <| List.foldl horizontallyCentered ( 0, [] ) subLayouts
                }


composeLayout : Config a -> C.Composed a -> Layout a
composeLayout config c =
    case c of
        C.Leaf a ->
            L.Item <| Box.init <| Just a

        C.Sequenced l r ->
            L.chainLayouts config <| List.map (toLayoutWithConfig config) [ l, r ]

        C.Aside a b ->
            L.parLayouts config <| List.map (toLayoutWithConfig config) [ a, b ]
