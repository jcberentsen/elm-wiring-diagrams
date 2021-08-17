module Diagram.Svg.Config exposing
    ( Config, forStringLabels, initWithLabelToString, default, withLabelToString
    , withTextAttributes, withTextAttributesFunction, withCellAttributes, withCellAttributesFunction
    )

{-| Configure how to convert Layout Labels to Strings for Svg output

@docs Config, forStringLabels, initWithLabelToString, default, withLabelToString
@docs withTextAttributes, withTextAttributesFunction, withCellAttributes, withCellAttributesFunction

-}

import Internal.Svg.Config as I
import Svg


{-| Preliminary way to control how labels turn into String for SVG
-}
type alias Config a msg =
    I.Config a msg


{-| Simple starting config. All labels will render as a dot

Use the Config modifiers to shape your intended Config

-}
default : Config a msg
default =
    I.default


{-| Simple starting config when labels are already String
-}
forStringLabels : Config String msg
forStringLabels =
    I.forStringLabels


{-| Configure a function to label a cell holding an `a`

The string will be displayed in the Svg boxes

-}
withLabelToString : (a -> String) -> Config a msg -> Config a msg
withLabelToString =
    I.withLabelToString


{-| Overload attributes for all cell label texts
-}
withTextAttributes : List (Svg.Attribute msg) -> Config a msg -> Config a msg
withTextAttributes attributes =
    I.withTextAttributes (always attributes)


{-| Overload attributes for any cell label text

You supply a function that looks at the label and decides which attributes to use

-}
withTextAttributesFunction : (a -> List (Svg.Attribute msg)) -> Config a msg -> Config a msg
withTextAttributesFunction func =
    I.withTextAttributes func


{-| Overload attributes for all cell boxes

These are the inner boxes with labels.

Only style colors, fonts.

Avoid modifying geometry, as this will likely mess up the layout

-}
withCellAttributes : List (Svg.Attribute msg) -> Config a msg -> Config a msg
withCellAttributes attributes =
    I.withBoxAttributes (always attributes)


{-| Overload attributes for each cell

You supply a function that looks at the label and decides which attributes to use

Only style colors, fonts.

Avoid modifying geometry, as this will likely mess up the layout

-}
withCellAttributesFunction : (Maybe a -> List (Svg.Attribute msg)) -> Config a msg -> Config a msg
withCellAttributesFunction func =
    I.withBoxAttributes func


{-| Init needs a labelToString function
-}
initWithLabelToString : (a -> String) -> Config a msg
initWithLabelToString =
    I.initWithLabelToString
