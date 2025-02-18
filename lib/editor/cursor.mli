type size
type shape = Square | Circle
type t

val make : shape -> size -> t

val size_exn : int -> size
val size_opt : int -> size option

val check : t -> int * int -> bool

val get_pos : t -> int * int
val set_pos : t -> int * int -> t
val move : t -> int * int -> t

val points : t -> (int * int) list

val grow : t -> t
val shrink : t -> t

