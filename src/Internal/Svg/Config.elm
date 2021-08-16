module Internal.Svg.Config exposing (..)

{-| Preliminary way to control how labels turn into String for SVG
-}


type Config a
    = Config
        { toLabelString : a -> String
        }


{-| Simple starting config. All labels will render as a dot

Use the Config modifiers to shape your intended Config

-}
default : Config a
default =
    Config { toLabelString = always "." }


{-| Simple starting config when labels are already String
-}
forStringLabels : Config String
forStringLabels =
    Config { toLabelString = identity }


{-| Simple starting config when labels are already String
-}
withLabelToString : (a -> String) -> Config a -> Config a
withLabelToString f (Config c) =
    Config { c | toLabelString = f }


{-| Init needs a labelToString function
-}
initWithLabelToString : (a -> String) -> Config a
initWithLabelToString f =
    Config { toLabelString = f }


{-| Apply the toLabelString function of the config to a value of type a
-}
applyToLabelString : Config a -> a -> String
applyToLabelString (Config c) a =
    c.toLabelString a
