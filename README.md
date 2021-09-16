# ![elm-wiring-diagrams](assets/png/logo.png)

# elm-wiring-diagrams

Wiring diagrams

This is an experimental package for creating wiring diagrams, inspired by <https://arxiv.org/pdf/2101.12046.pdf>

Diagrams can be composed of boxes with ports.
Boxes can be chained or put in parallel.

Diagrams are abstract, and needs to go through a Layout before rendering to Svg.

## Example usage

```elm
import Cartesian as C
import Cartesian.Layout as Layout
import Cartesian.Layout.Svg as Layout
import Cartesian.Svg as Svg
import Cartesian.Examples exposing (a,b,c)

svg = let diagram = a <| before b <| before c
  in Svg.view smallViewort <| Svg.fromDiagram diagram
```
