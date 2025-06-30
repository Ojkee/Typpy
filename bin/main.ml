open Notty_unix
open Lib

let () =
  let content = File_utils.file_content "data/words_alpha.txt" in
  let words = File_utils.content_words content 8 15 in
  let table = Lazy_table.create () in
  let term = Term.create () in
  let cols, rows = Term.size term in
  let max_text_width = 4 * cols / 5 in

  let letters = Letters.init_n_as_letters words 20 in

  let rec loop letters () =
    let frame =
      Render.typing_frame letters ~max_width:max_text_width ~cols ~rows
    in
    Term.image term frame;
    match Term.event term with
    | `Key (`Escape, _) -> ()
    | `Key (`ASCII p, _) -> loop (Letters.update_letters letters p) ()
    | `Key (`Backspace, _) -> loop (Letters.delete_last_current letters) ()
    | _ -> loop letters ()
  in

  loop letters ();
  Term.release term
