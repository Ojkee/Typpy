open Base

type int_type =
  | Finite of string
  | Infinite

type config_value =
  | Int of int_type
  | Bool of bool

type config_type =
  | WordsNumber
  | Punctuation
  | Capitalize
  | Adaptive

type config = {
  ctype : config_type;
  value : config_value;
  selected : bool;
}

type t = config list

let create () =
  [
    { ctype = WordsNumber; value = Int (Finite "5"); selected = true };
    { ctype = Punctuation; value = Bool false; selected = false };
    { ctype = Capitalize; value = Bool false; selected = false };
    { ctype = Adaptive; value = Bool false; selected = false };
  ]

let config_type_to_string = function
  | WordsNumber -> "number of words"
  | Punctuation -> "punctuation"
  | Capitalize -> "capitalize"
  | Adaptive -> "adaptive"

let config_value_to_string = function
  | Int (Finite x) -> x
  | Int Infinite -> "inf"
  | Bool x -> Bool.to_string x

let same_type ctype cfg_type =
  match (ctype, cfg_type) with
  | WordsNumber, WordsNumber -> true
  | Punctuation, Punctuation -> true
  | Capitalize, Capitalize -> true
  | _, _ -> false

let find_value configs cfg_type =
  List.find configs ~f:(fun cfg -> same_type cfg.ctype cfg_type)
  |> Option.value_exn ~message:"Config not found"
  |> fun { value; _ } -> value

let get_int_type configs cfgtype =
  match find_value configs cfgtype with
  | Int i -> Ok i
  | _ -> Error "Invalid type"

let get_bool configs cfgtype =
  find_value configs cfgtype |> fun x ->
  match x with
  | Bool x -> Ok x
  | _ -> Error "Invalid type"

let insert_value_cfg cfg c =
  let is_num = function
    | '0' .. '9' -> true
    | _ -> false
  in
  let is_space = function
    | ' ' -> true
    | _ -> false
  in
  match cfg with
  | { selected = false; _ } as cfg' -> cfg'
  | { value = Int (Finite x); _ } as cfg' when is_num c ->
      let new_int = x ^ String.make 1 c in
      if Int.of_string new_int > 1000 then
        { cfg' with value = Int (Finite "1000") }
      else { cfg' with value = Int (Finite new_int) }
  | { value = Int Infinite; _ } as cfg' when Char.( = ) c '0' -> cfg'
  | { value = Int Infinite; _ } as cfg' when is_num c ->
      let value = Int (Finite (String.make 1 c)) in
      { cfg' with value }
  | { value = Int _; _ } as cfg' -> cfg'
  | { value = Bool b; _ } as cfg' when is_space c ->
      { cfg' with value = Bool (not b) }
  | { value = Bool _; _ } as cfg' -> cfg'

let insert_value configs value =
  List.map configs ~f:(fun cfg -> insert_value_cfg cfg value)

let delete_value configs =
  let delete_value' = function
    | { selected = false; _ } as cfg' -> cfg'
    | { value = Int (Finite x); _ } as cfg' when String.length x = 1 ->
        { cfg' with value = Int Infinite }
    | { value = Int (Finite x); _ } as cfg' ->
        { cfg' with value = Int (Finite (String.drop_suffix x 1)) }
    | { value = Int Infinite; _ } as cfg' -> cfg'
    | { value = Bool _; _ } as cfg' -> cfg'
  in
  List.map configs ~f:delete_value'

let select_next configs =
  let selections (c : config) = c.selected in
  let shift lst =
    match List.rev lst with
    | ([] | [ _ ]) as r -> r
    | hd :: tl -> tl @ [ hd ] |> List.rev
  in
  List.map ~f:selections configs
  |> shift
  |> List.map2_exn ~f:(fun cfg sel -> { cfg with selected = sel }) configs

let cfg_to_letters { ctype; value; selected } ~max_width =
  let name = config_type_to_string ctype in
  let value_string = config_value_to_string value in
  let gap_len = max_width - String.length name - String.length value_string in
  let gap = String.make gap_len ' ' in
  [ name; value_string ] |> String.concat ~sep:gap
  |> Letters.of_string ~status:(if selected then SelectedText else Text)

let to_letters configs ~max_width =
  List.map configs ~f:(fun x -> cfg_to_letters x ~max_width)
