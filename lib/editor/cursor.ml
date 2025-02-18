type vector = int * int

let sub (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type shape = Square | Circle

type size = int

let size_exn i = 
  if i > 8 || i < 1 then
    failwith "Size must be between 1-8 inclusive"
  else i

let size_opt i =
  if i > 8 || i < 1 then
    None
  else Some i

type t = {
  shape : shape;
  position : vector;
  checker : (vector -> bool);
  size : size;
  bounds : size;
}

let make_square size = {
  shape = Square;
  position = (0, 0);
  checker = (fun (x, y) -> x < size && x >= 0 && y < size && y >= 0);
  size;
  bounds = size;
}

let make_circle size = {
  shape = Circle;
  position = (0, 0);
  checker = (fun (x, y) -> 
    let s_f = Float.of_int size in
    let x_f, y_f = Float.of_int (x - size + 1), Float.of_int (y - size + 1) in
    Float.sqrt ((x_f *. x_f) +. (y_f *. y_f)) < (s_f -. 0.7));
  size;
  bounds = size * size;
}

let check ({ position; checker; _} : t) pos = checker (sub pos position)

let get_pos ({ position; _} : t) = position

let make (shape : shape) (size : size)  =
  match shape with
  | Square -> make_square size
  | Circle -> make_circle size

let move ({ position; _ } as t : t) off = { t with position = add position off }

let set_pos t position = { t with position }

let points ({ position; checker; size; bounds; _ } : t) =
  let rec loop c r l = 
    if r > bounds then l
    else
      let c', r' = 
        if c = bounds then
          0, r + 1
        else
          c + 1, r
      in
      loop c' r' ((c, r) :: l)
  in 
  (loop 0 0 []) |> List.filter checker |> List.map (fun p -> add p position)

let grow t = 
  match (size_opt (t.size + 1)) with
  | None -> t
  | Some s -> set_pos (make t.shape s) t.position

let shrink t =
  match (size_opt (t.size - 1)) with
  | None -> t
  | Some s -> set_pos (make t.shape s) t.position
