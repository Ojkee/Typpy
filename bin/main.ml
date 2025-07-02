open Notty_unix
open Lib

let () =
  let term = Term.create () in
  let cols, rows = Term.size term in
  let max_width = 4 * cols / 5 in
  let n = 3 in

  let rec loop state =
    let frame = Render.frame state ~max_width ~rows ~cols in
    Term.image term frame;
    match Term.event term with
    | `Key (`Escape, _) -> ()
    | `Key (`Tab, _) -> loop (Window.handle_tab state)
    | `Key (`Enter, _) -> loop (Window.handle_enter state)
    | `Key (`ASCII c, _) -> loop (Window.handle_input_char state c)
    | `Key (`Backspace, _) -> loop (Window.handle_backspace state)
    | _ -> loop state
  in

  (* let window_state = Window.create_menu () in *)
  let window_state = Window.create () in
  loop window_state;
  Term.release term
