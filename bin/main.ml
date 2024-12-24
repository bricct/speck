open Notty
open Lwt.Infix

module Time = Core.Time_ns

module T = Notty_lwt.Term

let render = Editor.Render.render
let start = Editor.State.start
let update = Editor.Update.update

let timer = function
  | None   -> Lwt.wait () |> fst
  | Some t -> Lwt_unix.sleep t >|= fun () -> `Timer

let event e = Lwt_stream.get (T.events e) >|= function
  | Some (`Resize _ | #Unescape.event as x) -> `Event x
  | None -> `End

let listen () = Lwt_unix.sleep 3.14 >|= fun () -> `Network

let draw_screen term dim s frame =
  let new_frame = (render dim s) in
  let action () = 
    if not @@ I.equal new_frame frame then 
      T.image term (render dim s)
    else 
      Lwt.return_unit
  in
  action () >|= fun _ -> new_frame

let timed_term ?delay ~f initial_state =
  let term = T.create () in
  let socket = () in
  let rec loop (e, t, n) dim state frame =
    (Lwt.choose [e; t; n]) >>= function
    | `End | `Event `Key (`ASCII 'Q', [`Ctrl]) ->
        Lwt.return_unit
    | `Event `Resize dim as evt  -> invoke (event term, t, n) dim state evt frame
    | `Event e as evt     -> invoke (event term, t, n) dim state evt frame
    | `Network as evt     -> invoke (e, t, listen socket) dim state evt frame
    | `Timer as evt       -> invoke (e, timer delay, n) dim state evt frame
  and invoke es dim s e frame =
    match f dim s e with
    | `Continue s -> loop es dim s frame
    | `Stop       -> Lwt.return_unit
    | `Redraw s   ->
        draw_screen term dim s frame >>=
          fun new_frame ->
            loop es dim s new_frame
  in
  let size = T.size term in
  draw_screen term size initial_state I.empty >>= fun _ ->
    loop (event term, timer delay, listen socket) size initial_state (I.void 0 0)

let main () =
  timed_term ~delay:1.00 start
  ~f:(fun dim state e -> 
    let s', dirty = match e with 
    | `Event _ as evt -> update state evt
    | `Network as evt -> update state evt
    | `Timer as evt -> update state evt
    | _ -> state, false
    in
    if dirty then `Redraw s'
    else `Continue s'
  )
let () =
  Lwt_main.run @@ main ()


