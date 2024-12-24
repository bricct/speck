open Notty
open Movement
open Paint
open Event_listener

type state = State.t
type key = Unescape.key

let handle_char (state: state) key = 
  let old = (state, false) in
  match key with
  | 'v' -> { state with status = Select }, true
  | ' ' -> { state with canvas = Canvas.paint state.canvas @@ Registers.current state.registers; status = Pen }, true
  | 'u' -> { state with canvas = Canvas.undo state.canvas }, true
  | 'r' -> { state with canvas = Canvas.redo state.canvas }, true
  | 'x' -> { state with canvas = Canvas.paint state.canvas (0, 0, 0) }, true
  | _ -> old


let handle_specials (state: state) (k, mods) = match k with
  | `Backspace -> { state with canvas = Canvas.move_cursor state.canvas `Left }, true
  | `Tab -> { state with status = Colors }, true
  | _ -> state, false


let handle_key =
  handle_movement move_canvas_cursor >> 
  handle_paint select_color >> 
  char_handler handle_char >>
  handle_specials

