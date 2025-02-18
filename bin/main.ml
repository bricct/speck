open Notty
open Lwt.Infix

module State = Dust.State

module Time = Core.Time_ns

module T = Notty_lwt.Term

let render = Editor.Render.render
let start = Editor.State.start
let update = Editor.Update.update

type event = Editor.Update.event

let dust_state : (Editor.State.t, event) State.t = 
  State.return start
  |> State.add_stream (fun _ -> Lwt_unix.sleep 0.2 >|= fun _ -> `Animate)

let update dust_state evt =
  let s = State.get dust_state in
  let s', dirty = update s evt in
  State.set dust_state s', dirty

let () = Dust.run ~init:dust_state ~render ~update ()

