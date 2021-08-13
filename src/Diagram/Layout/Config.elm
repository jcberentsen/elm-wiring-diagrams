module Diagram.Layout.Config exposing (Config, default, setLeafExtent, setSpacing)

{-| Configuration for Layouts

@docs Config, default, setLeafExtent, setSpacing

-}

import Internal.Bound exposing (Bound)
import Internal.Layout.Config as I
import Internal.Vec2 exposing (Vec2)


{-| The Config type
-}
type alias Config a =
    I.Config a


{-| A default Config
-}
default : Config a
default =
    I.Config
        { spacing = Vec2 0 0
        , arrowLabler = \_ _ -> ""
        , leafExtent = always <| Just <| { lo = Vec2 0 0, hi = Vec2 30 20 }
        }


{-| Configure a function to compute the extent of a leaf node of a diagram from a value of type 'a'
-}
setLeafExtent : (a -> Bound) -> Config a -> Config a
setLeafExtent =
    I.setLeafExtent


{-| Configure the spacing between nodes in the diagram
-}
setSpacing : Vec2 -> Config a -> Config a
setSpacing s (I.Config c) =
    I.Config { c | spacing = s }
