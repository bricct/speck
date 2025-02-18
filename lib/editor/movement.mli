type state = State.t
type dir  = [ `Right | `Left | `Up | `Down ]
type t = (state -> dir -> int -> state) -> Event_listener.t

val move_canvas_cursor : state -> dir -> int -> state
val move_canvas_pen : state -> dir -> int -> state
val move_canvas_select : state -> dir -> int -> state
 
val move_palette_cursor : state -> dir -> int -> state
 
val handle_movement : t
