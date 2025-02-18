type state = State.t
type apply

val handle_paint : apply -> Event_listener.t

val canvas_apply : apply

val select_color : apply

val select_color_and_paint : apply

val set_color_register : apply
