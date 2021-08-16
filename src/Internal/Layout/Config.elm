module Internal.Layout.Config exposing (Config(..), initWithArrowLablerAndSpacing, setLeafExtent, spacing, leafExtent)

{-| Configuration for Layouts

@docs Config, initWithArrowLablerAndSpacing, setLeafExtent, spacing, leafExtent

-}

import Internal.Bound as Bound exposing (Bound)
import Internal.Vec2 exposing (..)


{-| The Config type
-}
type Config a
    = Config
        { spacing : Vec2
        , arrowLabler : ArrowLabler a
        , leafExtent : a -> Bound
        }


type alias ArrowLabler a =
    a -> a -> String


{-| Get the Vec2 representing the spacing between elements
-}
spacing : Config a -> Vec2
spacing (Config c) =
    c.spacing


{-| Initialize a Layout Configuration from an ArrowLabler and a spacing vector
-}
initWithArrowLablerAndSpacing : ArrowLabler a -> Vec2 -> Config a
initWithArrowLablerAndSpacing labler s =
    Config
        { spacing = s
        , arrowLabler = labler
        , leafExtent =
            always <|
                Bound.init <|
                    { lo = { x = 0, y = 0 }
                    , hi = { x = 20, y = 20 }
                    }
        }


{-| Configure the function to reserve the extents for objects of type a
-}
setLeafExtent : (a -> Bound) -> Config a -> Config a
setLeafExtent v (Config c) =
    Config { c | leafExtent = v }


{-| Run the leafExtent function of the configuration to get a Bound
-}
leafExtent : Config a -> a -> Bound
leafExtent (Config config) v =
    config.leafExtent v
