module Cartesian.Layout exposing (toLayout)

{-| Layout a cartesian structure


## Usage

@docs toLayout

-}

import Cartesian.Internal as C exposing (C(..))
import WiringDiagram.Layout.Internal as L exposing (Layout)


{-| Layout a cartesian structure

This is unfinished, and won't do anything interesting yet.

-}
toLayout : C a -> Layout a
toLayout c =
    case c of
        C.Unit ->
            L.empty

        C interface composition ->
            let
                inner =
                    composeLayout composition
            in
            L.empty


composeLayout : C.Composed a -> Layout a
composeLayout c =
    case c of
        C.Leaf _ ->
            L.empty

        C.Sequenced _ _ ->
            L.empty

        C.Aside _ _ ->
            L.empty
