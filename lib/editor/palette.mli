open Notty

type t

val make : unit -> t

val to_image : t -> image

val get_color : t -> Canvas.color

val move_cursor : t -> [`Up | `Down | `Left | `Right] -> t
