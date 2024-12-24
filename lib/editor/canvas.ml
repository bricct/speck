open Notty
open Base

type position = int * int

type cell = int * int * int

type color = cell

type fragment = { 
  col : int; 
  row : int;
  next : cell;
  curr : cell;
}

type update = fragment list

type t = {
  width : int;
  height : int;
  cells : cell array array;
  pos : position;
  select : position option;
  undo : update list;
  redo : update list;
}

let eq_color (r1, g1, b1) (r2, g2, b2) =
  r1 = r2 && g1 = g2 && b1 = b2

let is_light (r, g, b) = 
  r + g + b > 300

let white, black = (255,255,255), (0,0,0)

let apply cells update = 
  let undo_apply = List.fold update ~init:[] ~f:(fun acc { col; row; next; curr } ->
    if eq_color next curr then acc
    else
      let () = cells.(row).(col) <- next in
      { col; row; next=curr; curr=next } :: acc)
  in
  match undo_apply with
  | [] -> None
  | l -> Some l

let redo t = 
  match t.redo with
  | [] -> t
  | r :: rs -> 
      match apply t.cells r with
      | None -> t
      | Some l -> { t with undo = l :: t.undo; redo = rs }

let undo t =
  match t.undo with
  | [] -> t
  | u :: us ->
      match apply t.cells u with
      | None -> t
      | Some l -> { t with redo = l :: t.redo; undo = us }

let do_update t update =
  match apply t.cells update with 
  | None -> t
  | Some l -> { t with undo = l :: t.undo; redo = []}


let blank width height = {
  width;
  height = height * 2;
  cells = Array.make_matrix ~dimx:(height * 2) ~dimy:width (0, 0, 0);
  pos = (0, 0);
  select = None;
  undo = [];
  redo = [];
}

let apply_to_grid f (c1, r1) (c2, r2) = 
  let c_min, c_max = min c1 c2, max c1 c2 in
  let r_min, r_max = min r1 r2, max r1 r2 in
  for c = c_min to c_max do
    for r = r_min to r_max do
      f (c, r);
    done
  done

let fold t ~r_init ~c_init ~row ~col =
  let fold_col : ('b * int * int) -> 'c -> ('b * int * int) = 
    fun (acc, row_idx, col_idx) el ->
      (col acc (el, row_idx, col_idx), row_idx, col_idx + 1)
  in
  let fold_row : ('a * int) -> 'b -> ('a * int) =
    fun (acc, row_idx) el ->
      (row acc (el, row_idx), row_idx + 1)
  in
  let f : ('a * int) -> 'c array -> ('a * int) = fun (row_acc, row_idx) arr -> 
    let folded_col, _, _ = Array.fold ~init:(c_init, row_idx, 0) ~f:fold_col arr in
    fold_row (row_acc, row_idx) folded_col
  in
  fst @@ Array.fold ~init:(r_init, 0) ~f t

let to_image (t : t) pen_mode = 
  let col_pos, row_pos = t.pos in
  let row : (image * A.color list option) -> (A.color list * int) -> (image * A.color list option) = 
    fun (image, upper_colors) (color_list, row_idx) ->
      match upper_colors with
      | None -> (image, Some color_list)
      | Some uc ->
          let colors = List.rev @@ List.zip_exn uc color_list in
          let row_img = List.fold colors ~init:I.empty ~f:(fun acc (upper, lower) ->
            let i = I.uchar A.(bg upper ++ fg lower) (Stdlib.Uchar.of_int 0x2584) 1 1 in
            I.( acc <|> i))
          in
        I.(image <-> row_img), None
  in
  let cursor_color c = if is_light c then black else white in
  let col : A.color list -> (cell * int * int) -> A.color list = fun acc (color, row_idx, col_idx) ->
    let (r, g, b) = match t.select with
    | None -> if row_idx = row_pos && col_idx = col_pos && not pen_mode
        then cursor_color color
        else color
    | Some p ->
        let within (c1, r1) (c2, r2) (c, r) =
          let c_min, c_max = min c1 c2, max c1 c2 in
          let r_min, r_max = min r1 r2, max r1 r2 in
          c >= c_min && c <= c_max && r >= r_min && r <= r_max
        in
        if within p t.pos (col_idx, row_idx) then
          cursor_color color
        else
          color
    in
    A.rgb_888 ~r ~g ~b :: acc
  in
  fst @@ fold t.cells ~r_init:(I.empty, None) ~c_init:[] ~row ~col

let move t selector dir = 
  let bound d x = min (d - 1) x |> max 0 in
  let (x, y) = selector t in
  match dir with
  | `Up -> (x, (y - 1) |> bound t.height)
  | `Down -> (x, (y + 1) |> bound t.height) 
  | `Left -> ( (x - 1) |> bound t.width, y)
  | `Right -> ( (x + 1) |> bound t.width, y)

let move_cursor t dir = 
  let selector = fun t -> t.pos in
  let pos = move t selector dir in
  { t with pos }

let move_select t dir =
  let selector = fun t -> match t.select with
    | None -> t.pos
    | Some p -> p
  in
  let select = Some (move t selector dir) in
  { t with select }

let create_for_grid f (c1, r1) (c2, r2) =
  let min_c, max_c = min c1 c2, max c1 c2 in
  let min_r, max_r = min r1 r2, max r1 r2 in
  let rec loop c r l = 
    if r > max_r then l
    else
      let c', r' = 
        if c = max_c then
          min_c, r + 1
        else
          c + 1, r
      in
      loop c' r' (f (c, r) :: l)
  in loop min_c min_r []


let paint (t : t) color =
  let paint_fragment p =
    let col, row = p in
    let curr = t.cells.(row).(col) in
    { col; row; curr; next=color; }
  in
  let paint_select = create_for_grid paint_fragment in
  let update = match t.select with
  | None -> (paint_fragment t.pos :: [])
  | Some p -> paint_select p t.pos
  in
  do_update t update


(* let paint (t : t) color = 
  let paint_cell p = 
    let col, row = p in
    t.cells.(row).(col) <- color
  in
  let paint_select = apply_to_grid paint_cell in
  match t.select with
  | None -> paint_cell t.pos
  | Some p -> paint_select p t.pos
  *)
  
let deselect (t: t) =
  let pos = match t.select with
  | None -> t.pos
  | Some p -> p
  in
  { t with pos; select = None }
  

