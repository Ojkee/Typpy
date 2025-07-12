open Base

type letter_status =
  | Current
  | Pending
  | Correct
  | Mistake
  | Text
  | SelectedText
  | SummaryTable

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
  | SelectedText
  | SummaryTable ->
      { fg = { r = 255; g = 248; b = 231 }; bg = bg_color }
  | Correct -> { fg = { r = 128; g = 239; b = 128 }; bg = bg_color }
  | Mistake -> { fg = { r = 180; g = 100; b = 255 }; bg = bg_color }
  | Text -> { fg = { r = 153; g = 150; b = 141 }; bg = bg_color }

let style_of_letter ({ c = _; status } : letter) : style = status_style status

let is_rand_above_threshold ~chance =
  assert (Float.( <= ) 0. chance && Float.( <= ) chance 1.);
  Random.self_init ();
  let r = Random.float 1. in
  Float.( >= ) r chance

let maybe_capitalize word ~chance =
  if is_rand_above_threshold ~chance then word else String.capitalize word

let get_random_punctuation () =
  let p =
    [|
      "!";
      "\"";
      "#";
      "$";
      "%";
      "&";
      "'";
      "*";
      "+";
      ",";
      "-";
      ".";
      "/";
      ":";
      ";";
      "<";
      "=";
      ">";
      "?";
      "[";
      "\\";
      "^";
      "_";
      "`";
      "{";
      "|";
      "~";
    |]
  in
  Random.self_init ();
  let r = Random.int (Array.length p) in
  p.(r)

let maybe_punctuate word ~chance =
  if is_rand_above_threshold ~chance then word
  else
    match get_random_punctuation () with
    | ("\'" | "\"") as q -> q ^ word ^ q
    | "(" -> "(" ^ word ^ ")"
    | "{" -> "{" ^ word ^ "}"
    | "[" -> "[" ^ word ^ "]"
    | ("`" | "~" | "#") as prefix -> prefix ^ word
    | (">" | "<" | "+" | "*" | "\\") as infix -> word ^ " " ^ infix
    | p -> word ^ p

let to_letter_list lst =
  lst |> String.concat ~sep:" " |> String.to_list
  |> List.mapi ~f:(fun i c ->
         { c; status = (if i = 0 then Current else Pending) } )

let init_n_as_letters ~words ~n ~punctuation ~capitalize =
  let capitalize' word =
    if capitalize then maybe_capitalize word ~chance:0.2 else word
  in
  let punctuate' word =
    if punctuation then maybe_punctuate word ~chance:0.2 else word
  in
  Lazy_table.random_n_words words n
  |> List.map ~f:capitalize' |> List.map ~f:punctuate' |> to_letter_list

let init_from_list lst =
  let capitalize' word = maybe_capitalize word ~chance:1. in
  lst
  |> List.mapi ~f:(fun i word -> if i = 0 then capitalize' word else word)
  |> to_letter_list

let next_space (letters : t) : int =
  let rec aux acc = function
    | { c = ' '; _ } :: _
    | [] ->
        acc
    | _ :: tl -> aux (acc + 1) tl
  in
  aux 1 letters

let to_rows (letters : t) ~(max_width : int) : t list =
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

let update (letters : t) (pressed : char) : t =
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

let correct_count letters =
  List.count letters ~f:(fun { status; _ } ->
      match status with
      | Correct -> true
      | _ -> false )

let rec finished (letters : t) : bool =
  match letters with
  | [] -> true
  | { c = _; status = Current } :: _ -> false
  | _ :: tl -> finished tl

let exists letters ~f : bool = List.exists letters ~f

let current_row_idx letter_rows =
  let unwrap x =
    match x with
    | Some (i, _) -> i
    | None -> 0
  in
  let has_current _ x =
    exists x ~f:(fun { status; _ } ->
        match status with
        | Current -> true
        | _ -> false )
  in
  List.findi letter_rows ~f:has_current |> unwrap

let words_till_current letters =
  let rec aux acc = function
    | { c = ' '; status = Current } :: _ -> acc + 1
    | []
    | { status = Current; _ } :: _ ->
        acc
    | { c; _ } :: tl when Char.( = ) c ' ' -> aux (acc + 1) tl
    | _ :: tl -> aux acc tl
  in
  aux 0 letters

let is_current_f letters ~f =
  let rec aux = function
    | [] -> false
    | ({ status = Current; _ } as cur) :: _ -> f cur
    | _ :: tl -> aux tl
  in
  aux letters

let is_next_f letters ~f =
  let rec aux = function
    | [] -> false
    | { status = Current; _ } :: next :: _ -> f next
    | _ :: tl -> aux tl
  in
  aux letters

let is_space { c; _ } = Char.( = ) c ' '
