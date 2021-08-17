module Internal.Svg.Config exposing (..)

{-| Preliminary way to control how labels turn into String for SVG
-}

import Svg


type Config a msg
    = Config
        { toLabelString : a -> String
        , toTextAttributes : a -> List (Svg.Attribute msg)
        , toBoxAttributes : Maybe a -> List (Svg.Attribute msg)
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


{-| Init needs a labelToString function
-}
initWithLabelToString : (a -> String) -> Config a msg
initWithLabelToString f =
    default |> withLabelToString f
