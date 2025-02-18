open Notty 

type t = {
  frame : int;
  show_debug : bool;
  show_help : bool;
  debug : string;
  canvas : Canvas.t;
  palette : Palette.t;
  registers : Registers.t;
  status : Status.t;
  animate : int;
}

val start : t
