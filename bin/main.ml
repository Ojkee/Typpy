open Base
open Notty
open Notty_unix
open Lib.Lazy_table
open Lib.File_utils

type letter_status =
  | Current
  | Pending
  | Correct
  | Mistake

type letter = {
  c : char;
  mutable status : letter_status;
}

let backgound_color = A.rgb_888 ~r:51 ~g:51 ~b:51
let backgound_color_attr = A.(bg backgound_color)

let status_to_fg = function
  | Current -> backgound_color
  | Pending -> A.rgb_888 ~r:255 ~g:248 ~b:231
  | Correct -> A.rgb_888 ~r:128 ~g:239 ~b:128
  | Mistake -> A.rgb_888 ~r:170 ~g:0 ~b:255

let string_of_status = function
  | Current -> "Cur"
  | Pending -> "Pen"
  | Correct -> "Y"
  | Mistake -> "X"

let status_to_bg = function
  | Current -> A.rgb_888 ~r:255 ~g:248 ~b:231
  | _ -> backgound_color

let take_n_as_letters words n =
  LazyTable.random_n_words words n
  |> String.concat ~sep:" " |> String.to_list
  |> List.mapi ~f:(fun i c ->
         { c; status = (if i = 0 then Current else Pending) } )

let char_to_image ({ c; status } : letter) =
  I.string
    A.(fg (status_to_fg status) ++ bg (status_to_bg status) ++ st bold)
    (String.of_char c)

let next_space letters =
  let rec aux acc = function
    | { c = ' '; _ } :: _
    | [] ->
        acc
    | _ :: tl -> aux (acc + 1) tl
  in
  aux 1 letters

let rec to_rows current_row rows max_width = function
  | [] -> List.rev (current_row :: rows)
  | letters ->
      let next_space_n = next_space letters in
      let word, rest = List.split_n letters next_space_n in
      if next_space_n + List.length current_row <= max_width then
        to_rows (current_row @ word) rows max_width rest
      else to_rows word (current_row :: rows) max_width rest

let letters_to_image (letters : letter list) ~(max_width : int) =
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

let update_letters (letters : letter list) (pressed : char) =
  let get_status target got =
    if Char.compare target got = 0 then Correct else Mistake
  in
  let rec aux acc last_curr = function
    | [] -> List.rev acc
    | { c; status = Current } :: tl ->
        aux ({ c; status = get_status c pressed } :: acc) true tl
    | { c; _ } :: tl when last_curr ->
        aux ({ c; status = Current } :: acc) false tl
    | hd :: tl -> aux (hd :: acc) false tl
  in
  aux [] false letters

let delete_last_current letters =
  let rec aux acc last_curr = function
    | [] -> acc
    | { c; status = Current } :: tl ->
        aux ({ c; status = Pending } :: acc) true tl
    | { c; _ } :: tl when last_curr ->
        aux ({ c; status = Current } :: acc) false tl
    | hd :: tl -> aux (hd :: acc) false tl
  in
  aux [] false (List.rev letters)

let print_letters letters =
  letters
  |> List.map ~f:(fun { c; status } ->
         Printf.sprintf "{%c %s}" c (string_of_status status) )
  |> String.concat ~sep:" "
  |> fun x -> x ^ "\n" |> Stdio.print_string

let () =
  let content = FileUtils.file_content "data/words_alpha.txt" in
  let words = FileUtils.content_words content 8 15 in
  let table = LazyTable.create () in
  let term = Term.create () in
  let cols, rows = Term.size term in
  let max_text_width = 4 * cols / 5 in

  let letters = take_n_as_letters words 20 in

  let rec loop letters () =
    let frame = make_frame letters ~max_width:max_text_width cols rows in
    Term.image term frame;
    match Term.event term with
    | `Key (`Escape, _) -> ()
    | `Key (`ASCII p, _) -> loop (update_letters letters p) ()
    | `Key (`Backspace, _) -> loop (delete_last_current letters) ()
    | _ -> loop letters ()
  in

  loop letters ();
  Term.release term
