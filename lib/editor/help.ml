open Notty
open Styles

let title = "Speck Editor Help"



let normal = A.empty
let title = A.(st bold)
let control =  A.(fg (rgb_888 ~r:20 ~g:120 ~b:200))


type text_element = 
  | Span of string
  | Line of text_element list
  | Format of attr * text_element list
  | Break

let mk_control s = Format (control, [Span s])

let line s = Line [Span s]

let content = [
  Format (title, [line "Welcome to Speck!"]);
  Break;
  line "Speck is an unserious TUI pixel editor which aims to be your spontaneous outlet for creativity when you're jockeying the CLI.";
  Break;
  line "At it's core, Speck is just a grid of unicode half block characters which through the magic of abstraction become the pixels of a canvas.";
  Break;
  Line [Span "Each pixel can be colored by your brush by pressing "; mk_control "space"; 
        Span " to begin painting, using the "; mk_control "arrow keys"; 
        Span " to drag your brush around, and pressing "; mk_control "space"; Span " again to lift your brush."];
  line " Your brush can be an elegant pixelated circle for a classic and natural style, or a perfect square for a more exact and technical style.";
  Line [Span " To select a circular brush press "; mk_control "c"; 
        Span " or for a square brush press "; mk_control "s"; Span ".";];
  Line [Span " To increase or decrease the size of your brush, press "; mk_control "+";
        Span " or "; mk_control "-"; Span ".";];
  Break;
  line "The impressive color palette can be accessed at any time, and colors can be stored in your colors slots.";
  Line [Span " To toggle the color palette press "; mk_control "tab"; Span "."];
  Line [Span " In the color palette, you can select different colors using the "; mk_control "arrow keys"; Span", and assign them to a color slot by pressing a "; mk_control "number"; Span "."];
  Line [Span " On the main canvas screen, a brush color can be selected by pressing the "; mk_control "number"; Span " of the color slot."];
  Break;
  Line [Span "When your bout of spontaneity has passed, and you're ready to get back to creating shareholder value, you can exit with "; mk_control "Ctrl+Q"];
  Break;
]


(**
  * Takes a string s and an int width and returns a string list
  * containing substrings of s no greater in length that width
  *)
let chunk_string f s width = 
  let _ = if width < 5 then failwith "Width must be at least 5 for string chunking" else () in
  let rec chunk s acc = 
    match f s width with
    | lhs, Some rhs -> chunk rhs (lhs :: acc)
    | rem, None -> List.rev (rem :: acc)
  in
  chunk s []

(**
  * Rules for breaking strings
  * 1. The width must absolutely never be broken
  * 2. Words should be deferred entirely to the
  *    next line if the remaining space in the line
  *    is 3 or less, and the word has a length greater
  *    than the remaining space
  * 3. Up to one whitespace may be consumed at the
  *    beginning of a newline other than the first
  * 4. If a word has a length greater than the remaining
  *    space, and the remaining space is greater than 3
  *    the word should be split into part of size 
  *    (remaining_width - 1) and (string_width - 
  *    (remaining_width - 1)) and a hyphen should be
  *    added to the first part of the word
  * 5. The width must be at least 5
  *)

let text_wrap s width =
  let len = String.length s in
  if len <= width then s, None else
  let rec find_split_index i grace = 
    if i < 0 then 0, false, false else
    if String.get s i = ' ' then i, false, true else
    if grace = 0 then (width-1), true, false else
    find_split_index (i-1) (grace-1)
  in
  let i, hyphen, eat_whitespace = find_split_index width 4 in
  let lhs = 
    let plain = String.sub s 0 i in
    match hyphen with
    | false -> plain
    | true -> plain ^ "-"
  in
  let rhs = 
    let fi, li = match eat_whitespace with
    | true -> i + 1, (len  - (i + 1))
    | false -> i, (len - i) in
    String.sub s fi li
  in
  lhs, Some rhs


let fmt_content width =
  let rec render_line l img row fmt current_width  = 
    match l with
    | [] -> img, row, current_width
    | el :: els ->
        match el with
        | Span s -> 
            if current_width >= width then
              render_line els I.(img <-> (row <|> string fmt s)) I.empty fmt 0
            else
              let s1, s2 = text_wrap s (width - current_width) in
              (match s2 with
              | None -> render_line els img I.(row <|> string fmt s) fmt (current_width + String.length s1)
              | Some s2 -> render_line (Span s2 :: els) I.(img <-> (row <|> string fmt s1)) I.empty fmt 0)
        | Format (f, ls) ->
            let img, row, current_width = render_line ls img row f current_width in
            render_line els img row A.empty current_width 
        | Line ls ->
            let img, row, current_width = render_line ls img row fmt current_width in
            render_line els img row A.empty current_width 
        | Break -> 
            let w = width in
            render_line els I.(img <-> row <-> (I.void w 1)) I.empty fmt 0
  in
  let img, row, _ = render_line content I.empty I.empty normal 0 in
  I.(img <-> row)


let help_img = Outline.outline ~border:`Round ~attr:A.(bg black) (fmt_content 60)



(** TESTING *)

let s_to_chunk = "Hello world, this is a string that I would like to be formatted to fit within a certain space whilst minimizing the amount of times that a word must be hyphenated"

let%expect_test _ =
  let chunked_s = chunk_string text_wrap s_to_chunk 12 in
  let () = List.iter print_endline chunked_s in
  [%expect{|
    Hello world,
    this is a
    string that
    I would like
    to be forma-
    tted to fit
    within a
    certain spa-
    ce whilst
    minimizing
    the amount
    of times
    that a word
    must be hyp-
    henated
    |}]

let%expect_test _ = 
  let s_to_chunk = "Hello world, this is a string that I would like to be formatted to fit within a certain space whilst minimizing the amount of times that a word must be hyphenated" in
  let chunked_s = chunk_string text_wrap s_to_chunk 24 in
  let () = List.iter print_endline chunked_s in
  [%expect{|
    Hello world, this is a
    string that I would like
    to be formatted to fit
    within a certain space
    whilst minimizing the
    amount of times that a
    word must be hyphenated
    |}]

