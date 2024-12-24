open Notty

type t


val make : unit -> t

val set : t -> int -> Canvas.color -> unit

val get : t -> int -> Canvas.color

val current : t -> Canvas.color

val set_current : t -> int -> t

val to_image : t -> image



