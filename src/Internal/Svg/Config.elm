module Internal.Svg.Config exposing (..)

{-| Preliminary way to control how labels turn into String for SVG
-}

import Internal.Vec2 exposing (Vec2)
import Svg exposing (Svg)


type Config a msg
    = Config
        { toLabelString : a -> String
        , toTextAttributes : a -> List (Svg.Attribute msg)
        , toBoxAttributes : Maybe a -> List (Svg.Attribute msg)
        , wrapFunction : Maybe a -> List (Svg msg) -> Svg msg
        , labelPosition : LabelPositionFunction a
        }


{-| Simple starting config. All labels will render as a dot

Use the Config modifiers to shape your intended Config

-}
default : Config a msg
default =
    Config
        { toLabelString = always "."
        , toTextAttributes = always []
        , toBoxAttributes = always []
        , wrapFunction = always <| Svg.g []
        , labelPosition = labelCenter
        }


type alias LabelPositionFunction a =
    { lo : Vec2
    , width : Float
    , height : Float
    , radius : Float
    , label : Maybe a
    }
    -> Vec2


labelCenter : LabelPositionFunction a
labelCenter b =
    { x = b.lo.x + b.width / 2
    , y = b.lo.y + b.height / 2 -- * 3 / 5
    }


bottomLeft : LabelPositionFunction a
bottomLeft b =
    { x = b.lo.x + 4
    , y = b.lo.y + b.height - 4
    }


{-| Simple starting config when labels are already String
-}
forStringLabels : Config String msg
forStringLabels =
    default |> withLabelToString identity


{-| Simple starting config when labels are already String
-}
withLabelToString : (a -> String) -> Config a msg -> Config a msg
withLabelToString f (Config c) =
    Config { c | toLabelString = f }


withTextAttributes : (a -> List (Svg.Attribute msg)) -> Config a msg -> Config a msg
withTextAttributes attributes (Config c) =
    Config { c | toTextAttributes = attributes }


withBoxAttributes : (Maybe a -> List (Svg.Attribute msg)) -> Config a msg -> Config a msg
withBoxAttributes toAttributes (Config c) =
    Config { c | toBoxAttributes = toAttributes }


withCellWrapFunction :
    (Maybe a -> List (Svg msg) -> Svg msg)
    -> Config a msg
    -> Config a msg
withCellWrapFunction wrapFunction (Config c) =
    Config { c | wrapFunction = wrapFunction }


withLabelPositionFunction :
    LabelPositionFunction a
    -> Config a msg
    -> Config a msg
withLabelPositionFunction f (Config c) =
    Config { c | labelPosition = f }


{-| Init needs a labelToString function
-}
initWithLabelToString : (a -> String) -> Config a msg
initWithLabelToString f =
    default |> withLabelToString f
