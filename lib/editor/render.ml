open Notty
open Styles

let render (width, height) (state : State.t) = 
  let debug = if state.show_debug then 
    let attr = A.(bg green ++ fg black) in
    let outline_attr = A.(bg black ++ fg white) in
    let box = Styles.Outline.outline outline_attr @@ Layout.box ~width:20 ~height:5 attr @@ I.string attr state.debug in
    Layout.box ~width ~height:5 A.empty ~h_align:`Right box
  else
    I.void 0 0
  in
  let content =
    let height = height - 1 in
    let bg = A.(bg black) in
    let img = match state.status with
      | Colors -> Layout.flex_v ~gap:4 ~align:`Middle A.empty [Palette.to_image state.palette; Registers.to_image state.registers]
      | Normal | Pen | Select -> 
          let border_color = match state.status with
          | Pen  ->  A.red
          | _ -> A.white
          in
          Layout.flex_v ~gap:4 ~align:`Middle A.empty [Styles.Outline.outline A.(fg border_color) (Canvas.to_image state.canvas (state.status = Pen)); Registers.to_image state.registers]
    in
    Layout.box ~width ~height bg @@ img
  in
  let status = 
    let bg = A.(rgb_888 ~r:20 ~b:169 ~g:80 |> bg) in
    Layout.box ~width ~height:1 bg @@ I.string bg @@ Status.string_of_status state.status
  in
  I.((debug </> content) <-> status)
