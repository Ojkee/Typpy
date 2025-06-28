open Base

type letter_status =
  | Current
  | Pending
  | Correct
  | Mistake

type t = {
  c : char;
  status : letter_status;
}

type color = {
  r : int;
  g : int;
  b : int;
}

type style = {
  fg : color;
  bg : color;
}

let bg_color : color = { r = 51; g = 51; b = 51 }
let fg_color : color = { r = 255; g = 248; b = 231 }

let status_style = function
  | Current -> { fg = bg_color; bg = { r = 255; g = 248; b = 231 } }
  | Pending -> { fg = { r = 255; g = 248; b = 231 }; bg = bg_color }
  | Correct -> { fg = { r = 128; g = 239; b = 128 }; bg = bg_color }
  | Mistake -> { fg = { r = 170; g = 0; b = 255 }; bg = bg_color }

let string_of_status = function
  | Current -> "Cur"
  | Pending -> "Pen"
  | Correct -> "Y"
  | Mistake -> "X"

let style_of_letter ({ c = _; status } : t) : style = status_style status

let init_n_as_letters (words : string array) (n : int) : t list =
  Lazy_table.random_n_words words n
  |> String.concat ~sep:" " |> String.to_list
  |> List.mapi ~f:(fun i c ->
         { c; status = (if i = 0 then Current else Pending) } )

let next_space (letters : t list) : int =
  let rec aux acc = function
    | { c = ' '; _ } :: _
    | [] ->
        acc
    | _ :: tl -> aux (acc + 1) tl
  in
  aux 1 letters

let update_letters (letters : t list) (pressed : char) : t list =
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

let delete_last_current (letters : t list) : t list =
  let rec aux acc last_curr = function
    | [] -> acc
    | { c; status = Current } :: tl ->
        aux ({ c; status = Pending } :: acc) true tl
    | { c; _ } :: tl when last_curr ->
        aux ({ c; status = Current } :: acc) false tl
    | hd :: tl -> aux (hd :: acc) false tl
  in
  aux [] false (List.rev letters)

let print_letters (letters : t list) : unit =
  letters
  |> List.map ~f:(fun { c; status } ->
         Printf.sprintf "{%c %s}" c (string_of_status status) )
  |> String.concat ~sep:" "
  |> fun x -> x ^ "\n" |> Stdio.print_string
