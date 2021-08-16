module Diagram.Svg exposing (Viewport, largeViewport, mediumViewport, smallViewport, view, wideViewport)

{-| Some helpers for rendering diagrams to SVG

@docs Viewport, view, smallViewport, wideViewport, mediumViewport, largeViewport

-}

import Html exposing (Html)
import Internal.Svg as I
import Svg exposing (Svg)


{-| Placement and dimensions of a Viewport to render SVG inside
-}
type alias Viewport =
    I.Viewport


{-| Render a list of Svg items in a viewport to Html
-}
view : Viewport -> List (Svg msg) -> Html msg
view =
    I.view


{-| A small viewport of 200x200
-}
smallViewport : { width : number, height : number, xMin : number, yMin : number }
smallViewport =
    I.smallViewport


{-| A wide but low viewport of 1200x150
-}
wideViewport : { width : number, height : number, xMin : number, yMin : number }
wideViewport =
    I.wideViewport


{-| A medium sized viewport (640x480)
-}
mediumViewport : { width : number, height : number, xMin : number, yMin : number }
mediumViewport =
    I.mediumViewport


{-| A larger sized viewport (1024x768)
-}
largeViewport : { width : number, height : number, xMin : number, yMin : number }
largeViewport =
    I.largeViewport
