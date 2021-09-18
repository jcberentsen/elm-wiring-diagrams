# ![elm-wiring-diagrams](https://github.com/jcberentsen/elm-wiring-diagrams/blob/main/assets/png/logo.png?raw=true)

# elm-wiring-diagrams

Wiring diagrams

This is an experimental package for creating wiring diagrams, inspired by <https://arxiv.org/pdf/2101.12046.pdf>

Diagrams can be composed of boxes with ports.
Boxes can be chained or put in parallel.

Diagrams are abstract, and needs to go through a Layout before rendering to Svg.

## Example usage

Try out in Ellie:

https://ellie-app.com/fkhmBWKSmvVa1

```elm
module Main exposing (main)

import Cartesian as C exposing (before)

-- import modules for converting to svg and then to html
import Cartesian.Svg as Svg
import Diagram.Svg as Svg
-- Import some example diagrams
-- View source code:
-- https://github.com/jcberentsen/elm-wiring-diagrams/blob/main/src/Cartesian/Examples.elm
import Cartesian.Examples exposing (a,b,c)

svg = let diagram = a |> before b |> before c
  in Svg.view Svg.smallViewport [ Svg.fromDiagram diagram ]

main = svg
```

## Tutorial in Ellie:
https://ellie-app.com/fjxck9J4ktPa1
