open Notty_unix
open Lib

let () =
  let content = File_utils.file_content "data/words_alpha.txt" in
  let words = File_utils.content_words content 8 15 in
  let table = Lazy_table.create () in
  let term = Term.create () in
  let cols, rows = Term.size term in
  let max_width = 4 * cols / 5 in
  let n = 3 in

  let rec loop state =
    let frame = Render.frame state ~max_width ~rows ~cols in
    Term.image term frame;
    match Term.event term with
    | `Key (`Escape, _) -> ()
    | `Key (`ASCII p, _) -> loop (Window.input_update state p words n)
    | `Key (`Backspace, _) -> loop (Window.backspace_update state)
    | _ -> loop state
  in

  let window_state = Window.create_typing ~words ~n in
  loop window_state;
  Term.release term
