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
  let widths = List.map ~f:(fun word -> String.length word + 10) header in
  let mistakes_str =
    mistakes |> List.map ~f:to_string_list
    |> List.map ~f:(fun row -> render_row row widths)
  in
  render_separator widths :: render_row header widths :: render_separator widths
  :: mistakes_str
  @ [ render_separator widths ]

let info_table infos len = List.map infos ~f:(fun info -> pad info len)

let make_time_info execution_time num_letters len =
  let wpm =
    if Float.compare execution_time 0. <> 0 then
      Printf.sprintf "wpm: %.2f"
        (Int.to_float num_letters /. 5. *. (60. /. execution_time))
    else "0.00"
  in
  let time = Printf.sprintf "time: %.2fs" execution_time in
  let pad_len = 1 in
  let gap_len = len - String.length wpm - String.length time - (2 * pad_len) in
  [
    String.make pad_len ' ';
    wpm;
    String.make gap_len ' ';
    time;
    String.make pad_len ' ';
  ]
  |> String.concat ~sep:""

let render_summary_image { Window.mistakes; num_letters; execution_time } =
  let mistakes = Mistakes.common_counter_top_n mistakes 5 in
  let table = render_mistakes_table mistakes in
  let len = table |> List.hd |> Option.value ~default:"" |> String.length in
  let time_info = [ make_time_info execution_time num_letters len ] in
  let info = info_table [ "'r' to restart"; "'esc' to exit" ] len in
  List.map (time_info @ table @ info) ~f:Letters.of_string |> letter_rows_to_img

let render_menu { Window.configs } ~max_width =
  let cfg_to_letters { Window.name; value; selected } =
    let value_string = Window.config_value_to_string value in
    let gap_len = max_width - String.length name - String.length value_string in
    let gap = String.make gap_len ' ' in
    [ name; value_string ] |> String.concat ~sep:gap
    |> Letters.of_string ~status:(if selected then SelectedText else Text)
  in
  List.map configs ~f:cfg_to_letters |> letter_rows_to_img

let frame window ~max_width ~cols ~rows =
  let backgound_color_attr = A.(bg (rgb_888 ~r:51 ~g:51 ~b:51)) in
  let backgound = I.char backgound_color_attr ' ' cols rows in
  let draw_centered img =
    let centered = make_centered_image img cols rows in
    I.(centered </> backgound)
  in
  match window with
  | Window.Menu menu -> render_menu menu ~max_width |> draw_centered
  | Window.Typing { letters; _ } ->
      letters |> letters_to_image ~max_width |> draw_centered
  | Window.Summary summary -> render_summary_image summary |> draw_centered
