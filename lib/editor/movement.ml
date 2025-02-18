type state = State.t
type dir  = [ `Right | `Left | `Up | `Down ]
type t = (state -> dir -> int -> state) -> Event_listener.t

let move_canvas_cursor (state : state) dir amount = { state with canvas = Canvas.move_cursor state.canvas dir amount }

let move_canvas_pen (state : state) dir _ = 
  let color = Registers.current state.registers in
  let canvas = Canvas.move_cursor state.canvas dir 1 in
  { state with canvas = Canvas.paint canvas (Some color) }

let move_canvas_select (state : state) dir amount = { state with canvas = Canvas.move_select state.canvas dir amount }

let move_palette_cursor (state : state) dir amount =
  let palette = Palette.move_cursor state.palette dir in
  { state with palette }

let handle_movement (move_cursor : state -> dir -> int -> state) : Event_listener.t = fun s k ->
  let old = s, false in
  let handle_arrow = function
  | dir, [`Shift] -> move_cursor s dir 10, true
  | dir, _ -> move_cursor s dir 1, true
  in
  let handle_char = function
    | 'h' -> move_cursor s `Left 1, true
    | 'j' -> move_cursor s `Down 1, true
    | 'k' -> move_cursor s `Up 1, true
    | 'l' -> move_cursor s `Right 1, true
    | 'H' -> move_cursor s `Left 10, true
    | 'J' -> move_cursor s `Down 10, true
    | 'K' -> move_cursor s `Up 10, true
    | 'L' -> move_cursor s `Right 10, true
    | _ -> old
  in
  match k with
  | `ASCII c, mods -> handle_char c
  | `Arrow dir, mods -> handle_arrow (dir, mods)
  | _ -> old
