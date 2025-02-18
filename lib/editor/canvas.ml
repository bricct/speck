open Notty
open Base
open Core

type position = int * int

type color = int * int * int

type cell = color option

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
  cursor : Cursor.t;
  select : position option;
  undo : update list;
  redo : update list;
  anim_state : int;
}

let eq_color c1 c2 = 
  match c1, c2 with
  | None, None -> true
  | Some (r1, g1, b1), Some (r2, g2, b2) -> r1 = r2 && g1 = g2 && b1 = b2
  | _ -> false

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
  cells = Array.make_matrix ~dimx:(height * 2) ~dimy:width None;
  cursor = Cursor.make Cursor.Circle (Cursor.size_exn 1);
  select = None;
  undo = [];
  redo = [];
  anim_state = 0;
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
  let row : (image * A.color option list option) -> (A.color option list * int) -> (image * A.color option list option) = 
    fun (image, upper_colors) (color_list, row_idx) ->
      match upper_colors with
      | None -> (image, Some color_list)
      | Some uc ->
          let colors = List.rev @@ List.zip_exn uc color_list in
          let row_img = List.fold colors ~init:I.empty ~f:(fun acc (upper, lower) ->
            let uchar, attr = match lower, upper with
            | None, None -> 0x0020, A.empty
            | Some c, None -> 0x2584, A.(fg c)
            | None, Some c -> 0x2580, A.(fg c)
            | Some lc, Some uc -> 0x2584, A.(fg lc ++ bg uc)
            in
            let i = I.uchar attr (Stdlib.Uchar.of_int uchar) 1 1 in
            I.( acc <|> i))
          in
        I.(image <-> row_img), None
  in
  let cursor_color c = 
    let unblinked, blinked = match c with
    | None -> Some white, None
    | Some c -> match is_light c with 
      | true -> Some black, Some c
      | false -> Some white, Some c
    in
    if t.anim_state > 3 then
      blinked
    else
      unblinked
  in
  let col : A.color option list -> (cell * int * int) -> A.color option list = fun acc (color, row_idx, col_idx) ->
    let color = match t.select with
    | None -> if (Cursor.check t.cursor (col_idx, row_idx)) && not pen_mode
        then cursor_color color
        else color
    | Some p ->
        let within (c1, r1) (c2, r2) (c, r) =
          let c_min, c_max = min c1 c2, max c1 c2 in
          let r_min, r_max = min r1 r2, max r1 r2 in
          c >= c_min && c <= c_max && r >= r_min && r <= r_max
        in
        if within p (Cursor.get_pos t.cursor) (col_idx, row_idx) then
          cursor_color color
        else
          color
    in
    Option.(color >>| (fun (r, g, b) -> A.rgb_888 ~r ~g ~b)) :: acc
  in
  fst @@ fold t.cells ~r_init:(I.empty, None) ~c_init:[] ~row ~col

let in_bounds t (x, y) =
  x < t.width && x >= 0 && y < t.height && y >= 0

let move t selector dir amount = 
  match dir with
  | `Up -> (0, 0 - amount)
  | `Down -> (0, amount)
  | `Left -> (0 - amount, 0)
  | `Right -> (amount, 0)

let rec move_cursor t dir amount = 
  let selector = fun t -> Cursor.get_pos t.cursor in
  let vec = move t selector dir amount in
  let cursor = Cursor.move t.cursor vec in
  match List.filter (Cursor.points cursor) ~f:(in_bounds t) with
  | [] -> move_cursor t dir (amount - 1)
  | _ -> { t with cursor; anim_state = 0}

let move_select t dir amount =
  let selector = fun t -> match t.select with
    | None -> Cursor.get_pos t.cursor
    | Some p -> p
  in
  let select = Some (move t selector dir amount) in
  { t with select; anim_state = 0}

let paint (t : t) color =
  let paint_fragment p =
    let col, row = p in
    let curr = t.cells.(row).(col) in
    { col; row; curr; next=color; }
  in
  let points = Cursor.points t.cursor |> List.filter ~f:(in_bounds t) in
  let update = List.map points ~f:paint_fragment in
  do_update t update
  
let deselect (t: t) =
  { t with select = None }

let tick t () =
  { t with anim_state = (t.anim_state + 1) mod 6 }

let set_cursor t cursor = 
  match List.filter (Cursor.points cursor) ~f:(in_bounds t) with
  | [] -> t
  | _ -> { t with cursor; anim_state = 0}

let grow_cursor t =
  let cursor = Cursor.grow t.cursor in
  set_cursor t cursor

let shrink_cursor t =
  let cursor = Cursor.shrink t.cursor in
  set_cursor t cursor
