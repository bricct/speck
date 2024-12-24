open Notty
open Base

type state = State.t
type key = Unescape.key

type t = state -> key -> (state * bool)

let identity : t = fun s k -> (s, false)

let join : t -> t -> t = fun l1 l2 -> 
  fun s k -> 
    let s', dirty = l1 s k in
    if dirty then
      (s', true)
    else
      l2 s' k

let ( >> ) : t -> t -> t = join

let char_handler = fun f ->
  fun s (k, mods) -> 
    match k with
    | `ASCII c -> f s c
    | _ -> s, false

let return_state (s: state) = function
  | `ASCII 'C', [`Ctrl] -> { s with status = Normal; canvas = Canvas.deselect s.canvas }, true
  | _ -> s, false
