module Diagram.Svg.Config exposing (Config, forStringLabels, initWithLabelToString, withLabelToString, default)

{-| Configure how to convert Layout Labels to Strings for Svg output

@docs Config, forStringLabels, initWithLabelToString, withLabelToString, default

-}

-- exposing (Config, forStringLabels, initWithLabelToString, withLabelToString, default)

import Internal.Svg.Config as I


{-| Preliminary way to control how labels turn into String for SVG
-}
type alias Config a =
    I.Config a


{-| Simple starting config. All labels will render as a dot

Use the Config modifiers to shape your intended Config

-}
default : Config a
default =
    I.default


{-| Simple starting config when labels are already String
-}
forStringLabels : Config String
forStringLabels =
    I.forStringLabels


{-| Simple starting config when labels are already String
-}
withLabelToString : (a -> String) -> Config a -> Config a
withLabelToString =
    I.withLabelToString


{-| Init needs a labelToString function
-}
initWithLabelToString : (a -> String) -> Config a
initWithLabelToString =
    I.initWithLabelToString
