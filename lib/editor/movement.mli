type state = State.t
type dir  = [ `Right | `Left | `Up | `Down ]
type t = (state -> dir -> state) -> Event_listener.t

val move_canvas_cursor : state -> dir -> state
val move_canvas_pen : state -> dir -> state
val move_canvas_select : state -> dir -> state

val move_palette_cursor : state -> dir -> state

val handle_movement : t
