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

let starting_row letter_rows =
  let unwrap x =
    match x with
    | Some (i, _) -> i
    | None -> 0
  in
  let has_current x =
    Letters.exists x ~f:(fun { status; _ } ->
        match status with
        | Current -> true
        | _ -> false )
  in
  List.findi letter_rows ~f:(fun _ x -> has_current x) |> unwrap

let letter_rows_to_img ?n_rows letter_rows =
  let n = n_rows |> Option.value ~default:(List.length letter_rows) in
  let s = starting_row letter_rows |> fun s -> max 0 (s - (n / 2)) in
  letter_rows
  |> List.filteri ~f:(fun i _ -> s <= i && i < s + n)
  |> List.map ~f:Letters.to_list
  |> List.map ~f:row_to_image |> List.reduce ~f:I.( <-> )
  |> Option.value ~default:I.empty

let letters_to_image { Window.letters; _ } ~(max_width : int) =
  Letters.to_rows letters max_width |> letter_rows_to_img ~n_rows:5

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

let render_mistakes_table mistakes =
  let header = [ "Typed"; "Expected"; "Count" ] in
  let widths = List.map ~f:(fun word -> String.length word + 10) header in
  let mistakes_str =
    mistakes
    |> List.map ~f:Mistakes.mistake_to_string_list
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

let render_configs configs ~max_width =
  let cfg_to_letters { Window.ctype; value; selected } =
    let name = Window.config_type_to_string ctype in
    let value_string = Window.config_value_to_string value in
    let gap_len = max_width - String.length name - String.length value_string in
    let gap = String.make gap_len ' ' in
    [ name; value_string ] |> String.concat ~sep:gap
    |> Letters.of_string ~status:(if selected then SelectedText else Text)
  in
  List.map configs ~f:cfg_to_letters |> letter_rows_to_img

let render_typing window ~max_width = window |> letters_to_image ~max_width

let render_summary_image { Window.mistakes; num_letters; execution_time } =
  let mistakes = Mistakes.common_counter_top_n mistakes 5 in
  let table = render_mistakes_table mistakes in
  let len = table |> List.hd |> Option.value ~default:"" |> String.length in
  let time_info = [ make_time_info execution_time num_letters len ] in
  let info = info_table [ "'r' to restart"; "'esc' to exit" ] len in
  let of_string = Letters.of_string ~status:SummaryTable in
  List.map (time_info @ table @ info) ~f:of_string |> letter_rows_to_img

let menu_width cols = 2 * cols / 5
let typing_width cols = 4 * cols / 5

let backgound cols rows =
  I.char A.(bg (rgb_888 ~r:51 ~g:51 ~b:51)) ' ' cols rows

let frame window ~cols ~rows =
  let draw_centered img =
    let centered = make_centered_image img cols rows in
    I.(centered </> backgound cols rows)
  in
  match window.Window.current_state with
  | Window.Menu ->
      render_configs window.configs ~max_width:(menu_width cols)
      |> draw_centered
  | Window.Typing typing ->
      render_typing typing ~max_width:(typing_width cols) |> draw_centered
  | Window.Summary summary -> render_summary_image summary |> draw_centered
