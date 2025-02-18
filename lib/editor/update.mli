open Notty

type state = State.t
type event = [`Timer | `Animate | Dust.event ]

val update : state -> event -> state * bool
