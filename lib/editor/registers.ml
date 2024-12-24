open Notty
open Base
module Layout = Styles.Layout

type t = {
  arr : (int * int * int) array;
  current : int;
}

let black = (0, 0, 0)

let colors =
  let rec build idx l = match idx with
  | 0 -> l
  | n -> build (n-1) (black :: l)
  in
  build 10 []

let bound idx = 
  if idx > 9 || idx < 0 then
    invalid_arg "idx must be between 0-9"
  else if idx = 0 then 9
  else idx - 1

let make () = {
  arr = Array.of_list colors;
  current = 1
}

let get t idx = 
  Array.get t.arr @@ bound idx

let set t idx a =
  Array.set t.arr (bound idx) a

let current t = get t t.current

let set_current t idx = 
  { t with current = idx }

let to_image t = 
  let register_img color idx =
    let inv_bound = if idx = 9 then 0 else idx + 1 in
    let num_img = I.string A.empty @@ Int.to_string @@ inv_bound in
    let color_img = I.char A.(bg color) ' ' 3 2 |> Styles.Outline.outline A.(fg white) in
    Layout.flex_v ~align:`Middle A.empty [ num_img; color_img ]
  in

  let register_imgs, _ = Array.fold t.arr ~init:([], 0) ~f:(fun (acc, idx) (r, g, b) -> 
    let c = A.rgb_888 ~r ~g ~b in
    (register_img c idx :: acc, idx + 1)) 
  in
  let current_color = 
    let r, g, b = current t in
    A.rgb_888 ~r ~g ~b
  in
  let current_img = I.char A.(bg current_color) ' ' 6 2 |> Styles.Outline.outline A.(fg white) in
  let regs = Layout.flex_h ~gap:3 ~align:`Middle A.empty (List.rev register_imgs)
  |> Styles.Outline.outline ~border:`Open A.empty
  in
  Layout.flex_v ~gap:1 ~align:`Middle A.empty [current_img; regs]
  

  





