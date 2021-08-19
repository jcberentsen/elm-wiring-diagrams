module Diagram.Svg.Config exposing
    ( Config, forStringLabels, initWithLabelToString, default, withLabelToString
    , withTextAttributes, withTextAttributesFunction, withCellAttributes, withCellAttributesFunction
    , withCellWrappingFunction
    , withLabelPositionFunction, LabelPositionFunction, centered, bottomLeft
    )

{-| Configure how to convert Layout Labels to Strings for Svg output

@docs Config, forStringLabels, initWithLabelToString, default, withLabelToString
@docs withTextAttributes, withTextAttributesFunction, withCellAttributes, withCellAttributesFunction
@docs withCellWrappingFunction


## Control label positions

@docs withLabelPositionFunction, LabelPositionFunction, centered, bottomLeft

-}

import Internal.Svg.Config as I
import Svg


{-| Preliminary way to control how labels turn into String for SVG
-}
type alias Config a msg =
    I.Config a msg


{-| Type of a function that can determine a text position within a box

This can be used for centering and aligning labels towards edges or corners,

-}
type alias LabelPositionFunction a =
    I.LabelPositionFunction a


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


{-| Overload cell wrapping

Lets you configure how a list of inner Svg elements will be wrapped.

This allows you to set up onClick event attributes or using Svg.a for hyperlinking when the inner svg is clicked.

-}
withCellWrappingFunction :
    (Maybe a
     -> List (Svg.Svg msg)
     -> Svg.Svg msg
    )
    -> Config a msg
    -> Config a msg
withCellWrappingFunction wrapFunction =
    I.withCellWrapFunction wrapFunction


{-| Overload attributes for each cell

You supply a function that looks at the label and decides which attributes to use

Only style colors, fonts.

Avoid modifying geometry, as this will likely mess up the layout

-}
withCellAttributesFunction : (Maybe a -> List (Svg.Attribute msg)) -> Config a msg -> Config a msg
withCellAttributesFunction func =
    I.withBoxAttributes func


{-| Control the position of labels within boxes
The LabelPositionFunction must give positions that are inside the given box.
-}
withLabelPositionFunction :
    LabelPositionFunction a
    -> Config a msg
    -> Config a msg
withLabelPositionFunction =
    I.withLabelPositionFunction


{-| LabelPositionFunction for centering labels in box
-}
centered : LabelPositionFunction a
centered =
    I.labelCenter


{-| LabelPositionFunction for placing labels in lower left corner
-}
bottomLeft : LabelPositionFunction a
bottomLeft =
    I.bottomLeft


{-| Init needs a labelToString function
-}
initWithLabelToString : (a -> String) -> Config a msg
initWithLabelToString =
    I.initWithLabelToString
