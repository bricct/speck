type t = Normal | Select | Colors | Pen

let string_of_status = function
  | Normal -> "Normal"
  | Select -> "Select"
  | Colors -> "Colors"
  | Pen -> "Pen"
