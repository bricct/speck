open Event_listener

type state = State.t

type apply = state -> int -> state

let canvas_apply (state : state) register = 
  let color = Registers.get state.registers register in
  { state with canvas = Canvas.paint state.canvas color }

let select_color (state : state) register =
  let registers = Registers.set_current state.registers register in
  { state with registers }

let select_color_and_paint (state : state) register =
  canvas_apply (select_color state register) register

let set_color_register (state : state) register =
  let color = Palette.get_color state.palette in
  Registers.set state.registers register color;
  state


let handle_paint apply = 
  let handle_char state = function
  | '1' -> apply state 1, true
  | '2' -> apply state 2, true
  | '3' -> apply state 3, true
  | '4' -> apply state 4, true
  | '5' -> apply state 5, true
  | '6' -> apply state 6, true
  | '7' -> apply state 7, true
  | '8' -> apply state 8, true
  | '9' -> apply state 9, true
  | '0' -> apply state 0, true
  | '!' -> apply state 1, true
  | '@' -> apply state 2, true
  | '#' -> apply state 3, true
  | '$' -> apply state 4, true
  | '%' -> apply state 5, true
  | '^' -> apply state 6, true
  | '&' -> apply state 7, true
  | '*' -> apply state 8, true
  | '(' -> apply state 9, true
  | ')' -> apply state 0, true
  | _ -> state, false
  in
  char_handler handle_char



