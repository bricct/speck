type state = State.t
type dir  = [ `Right | `Left | `Up | `Down ]
type t = (state -> dir -> state) -> Event_listener.t

let move_canvas_cursor (state : state) dir = { state with canvas = Canvas.move_cursor state.canvas dir }

let move_canvas_pen (state : state) dir = 
  let color = Registers.current state.registers in
  let canvas = Canvas.move_cursor state.canvas dir in
  { state with canvas = Canvas.paint canvas color }

let move_canvas_select (state : state) dir = { state with canvas = Canvas.move_select state.canvas dir }

let move_palette_cursor (state : state) dir =
  let palette = Palette.move_cursor state.palette dir in
  { state with palette }

let handle_movement (move_cursor : state -> dir -> state) : Event_listener.t = fun s (k, _) ->
  let old = s, false in
  let handle_char = function
    | 'h' -> move_cursor s `Left, true
    | 'j' -> move_cursor s `Down, true
    | 'k' -> move_cursor s `Up, true
    | 'l' -> move_cursor s `Right, true
    | _ -> old
  in
  let handle_arrow dir = move_cursor s dir, true in
  match k with
  | `ASCII c -> handle_char c
  | `Arrow dir -> handle_arrow dir
  | _ -> old
