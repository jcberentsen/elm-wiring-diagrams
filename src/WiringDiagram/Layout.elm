module WiringDiagram.Layout exposing
    ( Layout, empty
    , layoutDiagram
    , layoutDiagramWithConfig
    , Polarity, portPositionsOfBox, incoming, outgoing
    )

{-| Layout of WiringDiagram values

Typically a Layout can be converted to SVG for display


# Definition

@docs Layout, empty


# Simple Layout of a Diagram

@docs layoutDiagram


# Customized Layout of a Diagram

@docs layoutDiagramWithConfig


# Misc

@docs Polarity, portPositionsOfBox, incoming, outgoing

-}

import Internal.WiringDiagram.Layout as I
import WiringDiagram exposing (..)
import WiringDiagram.Layout.Box exposing (Box, Pos)
import WiringDiagram.Layout.Config exposing (Config)


{-| The Layout type keeps information about the layout of a Wiring diagram.
-}
type alias Layout a =
    I.Layout a


{-| An empty Layout
-}
empty : Layout a
empty =
    I.empty


{-| Polarity of a Port. Is the port going In or Out of something
-}
type alias Polarity =
    I.Polarity


{-| Polarity of a Port that is incoming
-}
incoming : Polarity
incoming =
    I.In


{-| Polarity of a Port that is outgoing
-}
outgoing : Polarity
outgoing =
    I.Out


{-| Layout a Diagram with default configuration
-}
layoutDiagram : Diagram a -> Layout a
layoutDiagram =
    I.layoutDiagram


{-| Layout a Diagram with custom configuration
-}
layoutDiagramWithConfig : Config a -> Diagram a -> Layout a
layoutDiagramWithConfig =
    I.layoutDiagramWithConfig


{-| List the port positions of a Box, given a Polarity (In,Out)
-}
portPositionsOfBox : Polarity -> Box a -> List ( Int, Pos )
portPositionsOfBox =
    I.portPositionsOfBox
