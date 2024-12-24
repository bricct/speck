open Notty 

type t = {
  frame: int;
  show_debug: bool;
  debug : string;
  canvas : Canvas.t;
  palette : Palette.t;
  registers : Registers.t;
  status: Status.t;
}

val start : t
