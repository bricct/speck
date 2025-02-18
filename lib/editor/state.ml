open Notty

type position = int * int

type t = {
  frame: int;
  show_debug : bool;
  show_help : bool;
  debug : string;
  canvas : Canvas.t;
  palette : Palette.t;
  registers : Registers.t;
  status: Status.t;
  animate: int;
}

let start = { 
  frame = 0;
  debug = "";
  show_debug = false;
  show_help = false;
  canvas = Canvas.blank 64 24;
  palette = Palette.make ();
  registers = Registers.make ();
  status = Status.Normal;
  animate = 0;
}



