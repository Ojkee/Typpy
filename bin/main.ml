open Notty_unix
open Lib

let () =
  let term = Term.create () in
  let cols, rows = Term.size term in

  let rec loop window =
    let frame = Render.frame window ~rows ~cols in
    Term.image term frame;
    match Term.event term with
    | `Key (`Escape, _) -> ()
    | `Key (`Tab, _) -> loop (Window.handle_tab window)
    | `Key (`Enter, _) -> loop (Window.handle_enter window)
    | `Key (`ASCII c, _) -> loop (Window.handle_input_char window c)
    | `Key (`Backspace, _) -> loop (Window.handle_backspace window)
    | _ -> loop window
  in

  let window_state = Window.create () in
  loop window_state;
  Term.release term
