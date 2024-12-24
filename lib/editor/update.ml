open Notty

type state = State.t
type event = [ `Timer | `Network | `Event of [ Unescape.event | `Resize of int * int ]]


let handle_key (state : state) (key : Unescape.key) = 
  match state.status with 
  | Normal -> Normal.handle_key state key
  | Select -> Select.handle_key state key
  | Colors -> Colors.handle_key state key
  | Pen -> Pen.handle_key state key

let handle_event (state : state) = function
  | `Key (`ASCII '\\', _) -> { state with show_debug = not state.show_debug }, true
  | `Key k -> handle_key state k
  | `Resize _ -> state, true
  | _ -> state, false

let handle_network (state : state) : state = state
let handle_timer (state : state) : state = state

let update state evt = 
  match evt with
  | `Event evt -> handle_event { state with debug = "Event" } evt
  | `Network -> handle_network state, true
  | `Timer -> handle_timer state, true
