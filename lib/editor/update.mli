open Notty

type state = State.t
type event = [ `Timer | `Network | `Event of [ Unescape.event | `Resize of int * int ]]

val update : state -> event -> state * bool
