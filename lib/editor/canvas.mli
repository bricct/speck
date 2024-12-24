open Notty

type t
type color = int * int * int

val blank : int -> int -> t

val to_image : t -> bool -> image

val move_cursor : t -> [ `Up | `Down | `Left | `Right ] -> t
val move_select : t -> [ `Up | `Down | `Left | `Right ] -> t

val paint : t -> color -> t

val undo : t -> t
val redo : t -> t

val deselect : t -> t
