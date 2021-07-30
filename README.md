# elm-wiring-diagrams
Wiring diagrams

This is an experimental package for creating wiring diagrams, inspired by <https://arxiv.org/pdf/2101.12046.pdf>

Diagrams can be composed of boxes with ports.
Boxes can be chained or put in parallel.

Diagrams are abstract, and needs to go through a Layout before rendering to Svg.

## Example usage

```elm
import WiringDiagram exposing (inSequence)
import WiringDiagram.Svg as Svg
module WiringDiagram.Examples exposing (a,b,c)

svg = Svg.diagramToSvg <| inSequence [ a, b, c ]
```
