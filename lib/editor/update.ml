open Notty

type state = State.t
type event = [`Timer | `Animate | Dust.event ]

let handle_key (state : state) (key : Unescape.key) = 
  match state.status with 
  | Normal -> Normal.handle_key state key
  | Select -> Select.handle_key state key
  | Colors -> Colors.handle_key state key
  | Pen -> Pen.handle_key state key

let handle_animate (state : state) : state * bool = 
  let animate = (state.animate + 1) mod 6 in
  let canvas = match state.status with
  | Normal | Select -> Canvas.tick state.canvas ()
  | Pen | Colors -> state.canvas
  in
  { state with animate; canvas; debug = "Animate" }, true
let handle_timer (state : state) : state = state

let update (state : State.t) evt = 
  match evt with
  | `Key (`ASCII '\\', _) -> { state with show_debug = not state.show_debug }, true
  | `Key (`ASCII '?', _) -> { state with show_help = not state.show_help }, true
  | `Key k -> handle_key state k
  | `Resize _ -> state, true
  | `Animate -> handle_animate state
  | `Timer -> handle_timer state, true
  | _ -> state, false
