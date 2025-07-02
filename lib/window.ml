open Base

type int_type =
  | Finite of string
  | Infinite

type config_value =
  | Int of int_type
  | Bool of bool

type config = {
  name : string;
  value : config_value;
  selected : bool;
}

type menu = { configs : config list }

type typing = {
  letters : Letters.t;
  mistakes : Mistakes.t;
  start_time : float option;
}

type summary = {
  mistakes : Mistakes.t;
  num_letters : int;
  execution_time : float;
}

type t =
  | Menu of menu
  | Typing of typing
  | Summary of summary

let create_menu () =
  Menu
    {
      configs =
        [
          {
            name = "number of words";
            value = Int (Finite "5");
            selected = true;
          };
          { name = "punctuation"; value = Bool false; selected = false };
          { name = "uppercase"; value = Bool false; selected = false };
        ];
    }

let config_value_to_string = function
  | Int (Finite x) -> x
  | Int Infinite -> "inf"
  | Bool x -> Bool.to_string x

let create_typing ~words ~n =
  let letters = Letters.init_n_as_letters words n in
  let mistakes = Mistakes.create () in
  Typing { letters; mistakes; start_time = None }

let mistake_if_happened letters mistakes input =
  let make ?prefix ?suffix target =
    Mistakes.make_mistake ~inserted:input ~target ~prefix ~suffix
    |> Mistakes.add_mistake mistakes
  in
  let rec aux (lst : Letters.letter list) =
    match lst with
    | [] -> mistakes
    | { c = _; status = Current } :: _ -> mistakes
    | { c = target; status = Mistake } :: { c = after; status = Current } :: _
      ->
        make ~suffix:after target
    | { c = before; _ }
      :: { c = target; status = Mistake }
      :: { c = after; status = Current }
      :: _ ->
        make ~prefix:before ~suffix:after target
    | [ { c = before; _ }; { c = target; status = Mistake } ] ->
        make ~prefix:before target
    | [ { c = target; status = Mistake } ] -> make target
    | _ :: tl -> aux tl
  in
  aux (Letters.to_list letters)

let insert_value cfg c =
  let is_num = function
    | '0' .. '9' -> true
    | _ -> false
  in
  match cfg with
  | { selected = false; _ } as cfg' -> cfg'
  | { value = Int (Finite x); _ } as cfg' when is_num c ->
      let new_int = x ^ String.make 1 c in
      if Int.of_string new_int > 1000 then
        { cfg' with value = Int (Finite "1000") }
      else { cfg' with value = Int (Finite new_int) }
  | { value = Int Infinite; _ } as cfg' when is_num c ->
      let value = Int (Finite (String.make 1 c)) in
      { cfg' with value }
  | { value = Int _; _ } as cfg' -> cfg'
  | { value = Bool _; _ } as cfg' -> cfg'

let handle_menu_input_char { configs } c =
  List.map configs ~f:(fun cfg -> insert_value cfg c)

let handle_input_char state input words n =
  match state with
  | Menu menu -> Menu { configs = handle_menu_input_char menu input }
  | Typing { letters; mistakes; start_time; _ } -> (
      let letters = Letters.update_letters letters input in
      let mistakes = mistake_if_happened letters mistakes input in
      match (Letters.finished letters, start_time) with
      | false, None ->
          Typing { letters; mistakes; start_time = Some (Unix.gettimeofday ()) }
      | false, _ -> Typing { letters; mistakes; start_time }
      | true, Some start ->
          let num_letters = Letters.lenght letters in
          let execution_time = Unix.gettimeofday () -. start in
          Summary { mistakes; num_letters; execution_time }
      | true, None ->
          let num_letters = Letters.lenght letters in
          Summary { mistakes; num_letters; execution_time = 0. } )
  | Summary _ as s ->
      if Char.compare input 'r' = 0 then create_typing ~words ~n else s

let delete_value = function
  | { selected = false; _ } as cfg' -> cfg'
  | { value = Int (Finite x); _ } as cfg' when String.length x = 1 ->
      { cfg' with value = Int Infinite }
  | { value = Int (Finite x); _ } as cfg' ->
      { cfg' with value = Int (Finite (String.drop_suffix x 1)) }
  | { value = Int Infinite; _ } as cfg' -> cfg'
  | { value = Bool _; _ } as cfg' -> cfg'

let handle_backspace_menu { configs } = List.map configs ~f:delete_value

let handle_backspace state =
  match state with
  | Menu menu -> Menu { configs = handle_backspace_menu menu }
  | Typing { letters; mistakes; start_time } ->
      let letters = Letters.delete_last_current letters in
      Typing { letters; mistakes; start_time }
  | Summary _ as s -> s

let select_next_config { configs } =
  let selections (c : config) = c.selected in
  let shift lst =
    match List.rev lst with
    | ([] | [ _ ]) as r -> r
    | hd :: tl -> tl @ [ hd ] |> List.rev
  in
  List.map ~f:selections configs
  |> shift
  |> List.map2_exn ~f:(fun cfg sel -> { cfg with selected = sel }) configs

let handle_tab state =
  match state with
  | Menu menu -> Menu { configs = select_next_config menu }
  | Typing _ as s -> s
  | Summary _ as s -> s

let handle_enter state =
  match state with
  | Menu _ as s -> s
  | Typing _ as s -> s
  | Summary _ as s -> s
