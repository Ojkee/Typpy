open Base
open Notty

let char_to_image letter =
  let { Letters.fg = f; bg = b } = Letters.style_of_letter letter in
  let to_rgb ({ r; g; b } : Letters.color) = A.rgb_888 ~r ~g ~b in
  let letter_style = A.(fg (to_rgb f) ++ bg (to_rgb b) ++ st bold) in
  I.string letter_style (String.of_char letter.c)

let row_to_image row =
  row |> List.map ~f:char_to_image |> List.reduce ~f:I.( <|> )
  |> Option.value ~default:I.empty

let letter_rows_to_img (letter_rows : Letters.t list) =
  letter_rows
  |> List.map ~f:Letters.to_list
  |> List.map ~f:row_to_image |> List.reduce ~f:I.( <-> )
  |> Option.value ~default:I.empty

let letters_to_image (letters : Letters.t) ~(max_width : int) =
  Letters.to_rows letters max_width |> letter_rows_to_img

let make_centered_image image width height =
  let w = I.width image in
  let h = I.height image in
  let dx = max 0 ((width - w) / 2) in
  let dy = max 0 ((height - h) / 2) in
  I.pad ~l:dx ~t:dy image

let render_separator widths =
  let pieces = List.map widths ~f:(fun w -> String.make w '-') in
  "|-" ^ String.concat ~sep:"-+-" pieces ^ "-|"

let pad s len =
  let before = String.make ((len - String.length s) / 2) ' ' in
  let after = String.make (len - String.length s - String.length before) ' ' in
  before ^ s ^ after

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "| " ^ String.concat ~sep:" | " padded ^ " |"

let to_string_list ((i, t), count) =
  [ String.make 1 i; String.make 1 t; Int.to_string count ]

let render_mistakes_table mistakes =
  let header = [ "Typed"; "Expected"; "Count" ] in
  let widths = List.map ~f:String.length header in
  let mistakes_str =
    List.map mistakes ~f:to_string_list
    |> List.map ~f:(fun row -> render_row row widths)
  in
  render_row header widths :: render_separator widths :: mistakes_str

let info_table infos len = List.map infos ~f:(fun info -> pad info len)

let render_mistakes_image mistakes =
  let table = render_mistakes_table mistakes in
  let len = table |> List.hd |> Option.value ~default:"" |> String.length in
  let info = info_table [ "'r' to restart"; "'esc' to exit" ] len in
  List.map (table @ info) ~f:Letters.of_string |> letter_rows_to_img

let frame window ~max_width ~cols ~rows =
  let backgound_color_attr = A.(bg (rgb_888 ~r:51 ~g:51 ~b:51)) in
  let backgound = I.char backgound_color_attr ' ' cols rows in
  match window with
  | Window.Typing { letters; _ } ->
      let image = letters |> letters_to_image ~max_width in
      let centered = make_centered_image image cols rows in
      I.(centered </> backgound)
  | Window.Summary { mistakes } ->
      let top_mistakes = Mistakes.common_counter_top_n mistakes 5 in
      let mistakes_image = render_mistakes_image top_mistakes in
      let centered = make_centered_image mistakes_image cols rows in
      I.(centered </> backgound)
