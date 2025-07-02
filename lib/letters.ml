open Base

type letter_status =
  | Current
  | Pending
  | Correct
  | Mistake
  | Text
  | SelectedText

type letter = {
  c : char;
  status : letter_status;
}

type t = letter list

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
let create () = []
let rev (letters : t) : t = List.rev letters
let lenght (letters : t) : int = List.length letters
let of_list (x : letter list) : t = x
let to_list (x : t) : letter list = x

let of_string ?(status = Text) text =
  text |> String.to_list |> List.map ~f:(fun c -> { c; status })

let status_style = function
  | Current -> { fg = bg_color; bg = { r = 255; g = 248; b = 231 } }
  | Pending
  | Text ->
      { fg = { r = 255; g = 248; b = 231 }; bg = bg_color }
  | Correct -> { fg = { r = 128; g = 239; b = 128 }; bg = bg_color }
  | Mistake -> { fg = { r = 170; g = 0; b = 255 }; bg = bg_color }
  | SelectedText -> { fg = { r = 153; g = 150; b = 141 }; bg = bg_color }

let string_of_status = function
  | Current -> "Cur"
  | Pending -> "Pen"
  | Correct -> "Y"
  | Mistake -> "X"
  | Text -> "Txt"
  | SelectedText -> "Sxt"

let style_of_letter ({ c = _; status } : letter) : style = status_style status

let init_n_as_letters (words : string array) (n : int) : t =
  Lazy_table.random_n_words words n
  |> String.concat ~sep:" " |> String.to_list
  |> List.mapi ~f:(fun i c ->
         { c; status = (if i = 0 then Current else Pending) } )

let next_space (letters : t) : int =
  let rec aux acc = function
    | { c = ' '; _ } :: _
    | [] ->
        acc
    | _ :: tl -> aux (acc + 1) tl
  in
  aux 1 letters

let to_rows (letters : t) (max_width : int) : t list =
  let rec aux current_row rows = function
    | [] -> List.rev (current_row :: rows)
    | lst ->
        let next_space_n = next_space lst in
        let word, rest = List.split_n lst next_space_n in
        if next_space_n + List.length current_row <= max_width then
          aux (current_row @ word) rows rest
        else aux word (current_row :: rows) rest
  in
  aux [] [] letters

let update_letters (letters : t) (pressed : char) : t =
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

let delete_last_current (letters : t) : t =
  let rec aux acc last_curr = function
    | [] -> acc
    | [ ({ c = _; status = Current } as current) ] -> current :: acc
    | { c; status = Current } :: tl ->
        aux ({ c; status = Pending } :: acc) true tl
    | { c; _ } :: tl when last_curr ->
        aux ({ c; status = Current } :: acc) false tl
    | hd :: tl -> aux (hd :: acc) false tl
  in
  aux [] false (List.rev letters)

let rec finished (letters : t) : bool =
  match letters with
  | [] -> true
  | { c = _; status = Current } :: _ -> false
  | _ :: tl -> finished tl

let print_letters (letters : t) : unit =
  letters
  |> List.map ~f:(fun { c; status } ->
         Printf.sprintf "{%c %s}" c (string_of_status status) )
  |> String.concat ~sep:" "
  |> fun x -> x ^ "\n" |> Stdio.print_string
