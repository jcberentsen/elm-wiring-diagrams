module Diagram.Bound exposing (Bound, init, empty)

{-| Represent the horizontal and vertical ( possibly empty) extents of a two dimensional area.

@docs Bound, init, empty

-}

import Internal.Bound as I
import Internal.Extent exposing (Extent)


{-| The Bound type
-}
type alias Bound =
    I.Bound


{-| Initialize a Bound from an Extent
-}
init : Extent -> Bound
init =
    I.init


{-| Initialize an empty Bound
-}
empty : Bound
empty =
    I.empty
