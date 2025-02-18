open Notty

type t
type color = int * int * int

val blank : int -> int -> t

val to_image : t -> bool -> image

val move_cursor : t -> [ `Up | `Down | `Left | `Right ] -> int -> t
val move_select : t -> [ `Up | `Down | `Left | `Right ] -> int -> t

val grow_cursor : t -> t
val shrink_cursor : t -> t

val paint : t -> color option -> t

val tick : t -> unit -> t

val undo : t -> t
val redo : t -> t

val deselect : t -> t
