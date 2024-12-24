open Notty

type position = int * int

type t = {
  frame: int;
  show_debug: bool;
  debug : string;
  canvas : Canvas.t;
  palette : Palette.t;
  registers : Registers.t;
  status: Status.t;
}

let start = { 
  frame = 0;
  debug = "";
  show_debug = false;
  canvas = Canvas.blank 48 20;
  palette = Palette.make ();
  registers = Registers.make ();
  status = Status.Normal;
}



