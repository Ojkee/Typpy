open Base
open Notty
open Notty_unix
open Lib

let backgound_color = A.rgb_888 ~r:51 ~g:51 ~b:51
let backgound_color_attr = A.(bg backgound_color)

let char_to_image (letter : Letters.t) =
  let fg_color, bg_color =
    match Letters.style_of_letter letter with
    | { fg = None; bg = None } -> (Letters.fg_color, Letters.bg_color)
    | { fg = Some f; bg = None } -> (f, Letters.bg_color)
    | { fg = None; bg = Some b } -> (Letters.fg_color, b)
    | { fg = Some f; bg = Some b } -> (f, b)
  in
  let foo ({ r; g; b } : Letters.color) = A.rgb_888 ~r ~g ~b in
  I.string
    A.(fg (foo fg_color) ++ bg (foo bg_color) ++ st bold)
    (String.of_char letter.c)

let rec to_rows current_row rows max_width = function
  | [] -> List.rev (current_row :: rows)
  | letters ->
      let next_space_n = Letters.next_space letters in
      let word, rest = List.split_n letters next_space_n in
      if next_space_n + List.length current_row <= max_width then
        to_rows (current_row @ word) rows max_width rest
      else to_rows word (current_row :: rows) max_width rest

let letters_to_image (letters : Letters.t list) ~(max_width : int) =
  let row_to_image row =
    row |> List.map ~f:char_to_image |> List.reduce ~f:I.( <|> )
    |> Option.value ~default:I.empty
  in
  to_rows [] [] max_width letters
  |> List.map ~f:row_to_image |> List.reduce ~f:I.( <-> )
  |> Option.value ~default:I.empty

let make_centered_image image width height =
  let w = I.width image in
  let h = I.height image in
  let dx = max 0 ((width - w) / 2) in
  let dy = max 0 ((height - h) / 2) in
  I.pad ~l:dx ~t:dy image

let make_frame letters ~max_width cols rows =
  let image = letters |> letters_to_image ~max_width in
  let backgound = I.char backgound_color_attr ' ' cols rows in
  let centered = make_centered_image image cols rows in
  I.(centered </> backgound)

let () =
  let content = File_utils.file_content "data/words_alpha.txt" in
  let words = File_utils.content_words content 8 15 in
  let table = Lazy_table.create () in
  let term = Term.create () in
  let cols, rows = Term.size term in
  let max_text_width = 4 * cols / 5 in

  let letters = Letters.take_n_as_letters words 20 in

  let rec loop letters () =
    let frame = make_frame letters ~max_width:max_text_width cols rows in
    Term.image term frame;
    match Term.event term with
    | `Key (`Escape, _) -> ()
    | `Key (`ASCII p, _) -> loop (Letters.update_letters letters p) ()
    | `Key (`Backspace, _) -> loop (Letters.delete_last_current letters) ()
    | _ -> loop letters ()
  in

  loop letters ();
  Term.release term
