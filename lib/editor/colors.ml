open Notty
open Movement
open Paint
open Event_listener

type state = State.t
type key = Unescape.key



let handle_specials (state: state) (k, mods) = match k with
  | `Tab -> { state with status = Normal }, true
  | _ -> state, false


let handle_key =
  return_state >>
  handle_movement move_palette_cursor >> 
  handle_paint set_color_register >>
  handle_specials



