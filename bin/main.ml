open Notty_unix
open Lib

let () =
  let term = Term.create () in

  let rec loop window =
    let cols, rows = Term.size term in
    let frame = Render.frame window ~rows ~cols in
    Term.image term frame;
    match Term.event term with
    | `Key (`Escape, _) -> (
        match Window.handle_esc window with
        | None -> ()
        | Some window' -> loop window' )
    | `Key (`Tab, _) -> loop (Window.handle_tab window)
    | `Key (`Enter, _) -> loop (Window.handle_enter window)
    | `Key (`ASCII c, _) -> loop (Window.handle_input_char window c)
    | `Key (`Backspace, _) -> loop (Window.handle_backspace window)
    | `Resize _ -> loop window
    | _ -> loop window
  in

  let window_state = Window.create () in
  loop window_state;
  Term.release term
