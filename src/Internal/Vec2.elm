module Internal.Vec2 exposing (Vec2, translate)


type alias Vec2 =
    { x : Float
    , y : Float
    }


translate : Vec2 -> Vec2 -> Vec2
translate t v =
    { v | x = v.x + t.x, y = v.y + t.y }
