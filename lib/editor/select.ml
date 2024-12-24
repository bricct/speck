open Notty
open Paint
open Movement
open Event_listener

type state = State.t

let return_pixel_state (s: state) = function
  | `ASCII 'C', [`Ctrl] -> { s with status = Normal; canvas = Canvas.deselect s.canvas }, true
  | _ -> s, false

let handle_key = 
  return_pixel_state >>
  handle_paint canvas_apply >>
  handle_movement move_canvas_select
