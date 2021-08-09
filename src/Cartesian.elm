module Cartesian exposing
    ( C, Interface
    , before, aside, init, unit, group, interface
    )

{-| A module for cartesian structure

This is an experiment in organizing wiring diagrams

We're not implementing this paper exactly, but finding some inspiration:

The Cost of Compositionality: A High-Performance Implementation of String Diagram Composition

<https://arxiv.org/abs/2105.09257>


## Types

@docs C, Interface


## Usage

@docs before, aside, init, unit, group, interface

-}

import Internal.Cartesian as Internal exposing (..)
import Internal.Cartesian.Interface as I exposing (Interface)


{-| The type of a cartesian structure

This is not your typical category theory monoidal thing, as we store a recursive structure

-}
type alias C a =
    Internal.C a


{-| The Interface type describes the Arity of the cartesian system structure
-}
type alias Interface =
    I.Interface


{-| The unit value

This is possibly not needed, but we'll start off using it.

This may disappear later.

-}
unit : C a
unit =
    Internal.Unit


{-| Group a structure

This may be used to group substructures

-}
group : C a -> C a
group =
    Internal.group


{-| Initialize a simple 1->1 arity morphism with label 'a'
-}
init : a -> C a
init =
    Internal.init


{-| Put one structure before another

The structures will be dependent and must match in the interface

Note the order of arguments are flipped to support this syntax:

    a |> before b

This will put `a` 'first'

-}
before : C a -> C a -> C a
before =
    Internal.before


{-| Put two structures aside each other

The structures will be independent

Note the order of arguments are flipped to support this syntax:

    a |> aside b

This will put `a` 'first'

-}
aside : C a -> C a -> C a
aside =
    Internal.aside


{-| Extract the outermost interface of the structure
-}
interface : C a -> Interface
interface =
    Internal.interface
