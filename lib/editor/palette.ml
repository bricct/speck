open Notty
open Base

type position = int * int

type t = {
  height: int;
  width: int;
  pos: position;
}

let make () = { height = 20; width = 33; pos = (0, 0) }


type rgb = int * int * int


let get_gradient color1 color2 divisions = 
  let step = 1. /. Float.of_int divisions in
  let rec colors l n =
    match n with
    | 0 -> l
    | x -> colors (Color_utils.interpolate color1 color2 (Float.of_int n *. step) :: l) (n - 1)
  in
  colors [] divisions

let get_color_gradient (r, g, b) d = get_gradient (r, g, b) (r / 20, g / 20, b / 20) d

let c_rainbow w =
  let pi2     = 2. *. 3.14159 in
  let pi2_3   = pi2 /. 3.
  and f t off = Float.sin (t +. off) *. 128. +. 128. |> Int.of_float in
  let color t = (f t (-.pi2_3)), (f t 0.), (f t pi2_3) in
  let rec get_columns l step =
    let t = (pi2 *. Float.of_int step /. Float.of_int w) +. 3.7 in
    match step with
    | 0 -> l
    | x -> get_columns (color t :: l) (step - 1)
  in
  get_columns [] w


let map = 
  let color_list_matrix = 
    let headers = c_rainbow 32 in
    List.fold_left headers ~init:[] ~f:(fun acc c -> get_color_gradient c 20 :: acc) 
  in
  let grayscale = get_gradient (255, 255, 255) (0, 0, 0) 20 in
  grayscale :: color_list_matrix
    |> List.transpose_exn


let move_cursor t dir = 
  let bound d x = min (d - 1) x |> max 0 in
  let (x, y) = t.pos in
  let pos = match dir with
  | `Up -> (x, (y - 1) |> bound t.height)
  | `Down -> (x, (y + 1) |> bound t.height) 
  | `Left -> ( (x - 1) |> bound t.width, y)
  | `Right -> ( (x + 1) |> bound t.width, y)
  in
  { t with pos }

let get_color t =
  let c, r = t.pos in
  List.nth_exn (List.nth_exn map r) c
  


let fold t ~r_init ~c_init ~row ~col =
  let fold_col : ('b * int * int) -> 'c -> ('b * int * int) = 
    fun (acc, row_idx, col_idx) el ->
      (col acc (el, row_idx, col_idx), row_idx, col_idx + 1)
  in
  let fold_row : ('a * int) -> 'b -> ('a * int) =
    fun (acc, row_idx) el ->
      (row acc (el, row_idx), row_idx + 1)
  in
  let f : ('a * int) -> 'c list -> ('a * int) = fun (row_acc, row_idx) l -> 
    let folded_col, _, _ = List.fold ~init:(c_init, row_idx, 0) ~f:fold_col l in
    fold_row (row_acc, row_idx) folded_col
  in
  fst @@ List.fold ~init:(r_init, 0) ~f t

let to_image t =
  let row acc (el, _) = 
    I.(acc <-> el)
  in
  let col acc ((r, g, b), row_idx, col_idx) =
    let color = A.rgb_888 ~r ~g ~b in
    let inverse = if r + b + g > 300 then A.black else A.white in
    let c, r = t.pos in
    let i = 
      if row_idx = r && col_idx = c then
        let left = I.uchar A.(fg inverse ++ bg color) (Stdlib.Uchar.of_int 0x276E) 1 1 in
        let right = I.uchar A.(fg inverse ++ bg color) (Stdlib.Uchar.of_int 0x276F) 1 1 in
        I.( left <|> right)
      else 
        I.char A.(bg color) ' '  2 1
      in
    I.(acc <|> i) 
  in

  fold map ~r_init:I.empty ~c_init:I.empty ~row ~col

