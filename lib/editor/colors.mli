open Notty

type state = State.t
type key = Unescape.key


val handle_key : Event_listener.t
