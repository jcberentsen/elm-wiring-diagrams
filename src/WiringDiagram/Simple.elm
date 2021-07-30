module Common.WiringDiagram.Simple exposing
    ( Diagram
    , dia
    , wrap
    )

import Common.WiringDiagram as D


type alias Diagram =
    D.Diagram String


dia =
    D.init


wrap : Diagram -> Diagram
wrap =
    D.initWrap
