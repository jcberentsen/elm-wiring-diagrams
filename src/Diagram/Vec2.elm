module Diagram.Vec2 exposing (Vec2, init, translate)

{-| A simple Vec2 type for geometry

@docs Vec2, init, translate

-}

import Internal.Vec2 as I


{-| A simple Vec2 type
-}
type alias Vec2 =
    I.Vec2


{-| Initialize a Vec2 from two floats
-}
init : Float -> Float -> Vec2
init x y =
    { x = x, y = y }


{-| Translate one vector by another (add)
-}
translate : Vec2 -> Vec2 -> Vec2
translate =
    I.translate
