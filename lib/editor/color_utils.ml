type rgb = { r : float; g : float; b : float }
type hsv = { h : float; s : float; v : float }

(* Clamp a value to the range [0, 255] *)
let clamp value = min 255 (max 0 value)

(* Normalize RGB from [0, 255] to [0.0, 1.0] *)
let normalize_rgb (r, g, b) =
  {
    r = float_of_int (clamp r) /. 255.0;
    g = float_of_int (clamp g) /. 255.0;
    b = float_of_int (clamp b) /. 255.0;
  }

(* Denormalize RGB from [0.0, 1.0] to [0, 255] *)
let denormalize_rgb ({ r; g; b }: rgb) =
  let to_int_and_clamp x =
    clamp (Int.of_float (x *. 255.0))
  in
  (to_int_and_clamp r, to_int_and_clamp g, to_int_and_clamp b)


let rgb_to_hsv ({r; g; b }: rgb) : hsv = 
  let max_color = max r g |> max b in
  let min_color = min r g |> min b in
  let chroma = max_color -. min_color in
  let h = 
    if chroma = 0.0 then 0.0
    else if max_color = r then 
      60.0 *. (((g -. b) /. chroma) +. (if g < b then 6.0 else 0.0))
    else if max_color = g then
      60.0 *. (((b -. r) /. chroma) +. 2.0)
    else
      60.0 *. (((r -. g) /. chroma) +. 4.0)
  in
  let s = if max_color = 0.0 then 0.0 else chroma /. max_color in
  let v = max_color in 
  {h; s; v}


let hsv_to_rgb ({ h; s; v }: hsv) : rgb =
  let chroma = v *. s in
  let h' = h /. 60.0 in
  let x = chroma *. (1.0 -. abs_float (mod_float h' 2.0 -. 1.0)) in
  let (r1, g1, b1) =
    if h' < 1.0 then (chroma, x, 0.0)
    else if h' < 2.0 then (x, chroma, 0.0)
    else if h' < 3.0 then (0.0, chroma, x)
    else if h' < 4.0 then (0.0, x, chroma)
    else if h' < 5.0 then (x, 0.0, chroma)
    else (chroma, 0.0, x)
  in
  let m = v -. chroma in
  { r = r1 +. m; g = g1 +. m; b = b1 +. m }

(* Interpolate between two HSV colors *)
let interpolate_hsv c1 c2 t =
  let lerp a b t = a +. t *. (b -. a) in
  let h1, h2 = c1.h, c2.h in
  let delta_h =
    if abs_float (h2 -. h1) > 180.0 then
      if h2 > h1 then h2 -. 360.0 else h2 +. 360.0
    else h2
  in
  let h = mod_float (lerp h1 delta_h t) 360.0 in
  let s = lerp c1.s c2.s t in
  let v = lerp c1.v c2.v t in
  { h; s; v }

(* Full interpolation: RGB -> HSV -> Interpolate -> HSV -> RGB *)
let interpolate_rgb c1 c2 t =
  let hsv1 = rgb_to_hsv c1 in
  let hsv2 = rgb_to_hsv c2 in
  let interpolated_hsv = interpolate_hsv hsv1 hsv2 t in
  hsv_to_rgb interpolated_hsv


let interpolate c1 c2 t = 
  let rgb1 = normalize_rgb c1 in
  let rgb2 = normalize_rgb c2 in
  interpolate_rgb rgb1 rgb2 t 
  |> denormalize_rgb
