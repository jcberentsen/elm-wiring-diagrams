module WiringDiagram.Svg.Config exposing
    ( Config, forStringLabels, initWithLabelToString, withLabelToString
    , applyToLabelString
    )

{-| Configure how to convert Layout Labels to Strings for Svg output

@docs Config, forStringLabels, initWithLabelToString, withLabelToString


# Applying the config (You probably won't need to use this, it will be used by `Svg.layoutToSvgWithConfig`)

@docs applyToLabelString

-}


{-| Preliminary way to control how labels turn into String for SVG
-}
type Config a
    = Config
        { toLabelString : a -> String
        }


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
