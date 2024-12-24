open Notty
open Base

type state = State.t
type key = Unescape.key

type t = state -> key -> (state * bool)


val identity : t

val join : t -> t -> t

val ( >> ) : t -> t -> t

val char_handler : (state -> char -> (state * bool)) -> t

val return_state : t
