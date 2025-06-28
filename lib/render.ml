open Base
open Notty

let char_to_image letter =
  let { Letters.fg = f; bg = b } = Letters.style_of_letter letter in
  let to_rgb ({ r; g; b } : Letters.color) = A.rgb_888 ~r ~g ~b in
  let letter_style = A.(fg (to_rgb f) ++ bg (to_rgb b) ++ st bold) in
  I.string letter_style (String.of_char letter.c)

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

let make_frame letters ~max_width ~cols ~rows =
  let backgound_color_attr = A.(bg (rgb_888 ~r:51 ~g:51 ~b:51)) in
  let image = letters |> letters_to_image ~max_width in
  let backgound = I.char backgound_color_attr ' ' cols rows in
  let centered = make_centered_image image cols rows in
  I.(centered </> backgound)
