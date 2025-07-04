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
  | Uppercase

type config = {
  ctype : config_type;
  value : config_value;
  selected : bool;
}

type configs = config list

type typing = {
  letters : Letters.t;
  current_row : int;
  mistakes : Mistakes.t;
  start_time : float option;
}

type summary = {
  mistakes : Mistakes.t;
  num_letters : int;
  execution_time : float;
}

type state =
  | Menu
  | Typing of typing
  | Summary of summary

type lexicon = { words : Words.t (* memo : Lazy_table.t; *) }

type t = {
  current_state : state;
  lexicon : lexicon;
  configs : configs;
}

let config_type_to_string = function
  | WordsNumber -> "number of words"
  | Punctuation -> "punctuation"
  | Uppercase -> "uppercase"

let config_value_to_string = function
  | Int (Finite x) -> x
  | Int Infinite -> "inf"
  | Bool x -> Bool.to_string x

let create_default_configs () =
  [
    { ctype = WordsNumber; value = Int (Finite "5"); selected = true };
    { ctype = Punctuation; value = Bool false; selected = false };
    { ctype = Uppercase; value = Bool false; selected = false };
  ]

let same_ctype ctype cfg_type =
  match (ctype, cfg_type) with
  | WordsNumber, WordsNumber -> true
  | Punctuation, Punctuation -> true
  | Uppercase, Uppercase -> true
  | _, _ -> false

let find_config cfg_type configs =
  List.find configs ~f:(fun cfg -> same_ctype cfg.ctype cfg_type)
  |> Option.value_exn ~message:"Config not found"
  |> fun { value; _ } ->
  match value with
  | Int x -> x
  | _ -> assert false

let create_typing { lexicon = { words; _ }; configs; _ } =
  let num_words = find_config WordsNumber configs in
  let n =
    match num_words with
    | Finite x -> Int.of_string x
    | Infinite -> 100
  in
  let letters = Letters.init_n_as_letters words n in
  let mistakes = Mistakes.create () in
  Typing { letters; current_row = 0; mistakes; start_time = None }

let create () =
  let words = Words.create ~file_name:"data/words_alpha.txt" ~min:8 ~max:15 in
  (* let memo = Lazy_table.create () in *)
  let current_state = Menu in
  let configs = create_default_configs () in
  { current_state; lexicon = { words (* ; memo *) }; configs }

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

let handle_menu_input_char { configs; _ } c =
  List.map configs ~f:(fun cfg -> insert_value cfg c)

let handle_input_char window input : t =
  let update_state state = { window with current_state = state } in
  match window.current_state with
  | Menu -> { window with configs = handle_menu_input_char window input }
  | Typing ({ letters; mistakes; start_time; _ } as typing) -> (
      let letters = Letters.update letters input in
      let mistakes = mistake_if_happened letters mistakes input in
      match (Letters.finished letters, start_time) with
      | false, None ->
          update_state
            (Typing
               {
                 typing with
                 letters;
                 mistakes;
                 start_time = Some (Unix.gettimeofday ());
               } )
      | false, _ -> update_state (Typing { typing with letters; mistakes })
      | true, Some start ->
          let num_letters = Letters.lenght letters in
          let execution_time = Unix.gettimeofday () -. start in
          update_state (Summary { mistakes; num_letters; execution_time })
      | true, None ->
          let num_letters = Letters.lenght letters in
          update_state (Summary { mistakes; num_letters; execution_time = 0. })
      )
  | Summary _ ->
      if Char.( = ) input 'r' then
        { window with current_state = create_typing window }
      else window

let delete_value = function
  | { selected = false; _ } as cfg' -> cfg'
  | { value = Int (Finite x); _ } as cfg' when String.length x = 1 ->
      { cfg' with value = Int Infinite }
  | { value = Int (Finite x); _ } as cfg' ->
      { cfg' with value = Int (Finite (String.drop_suffix x 1)) }
  | { value = Int Infinite; _ } as cfg' -> cfg'
  | { value = Bool _; _ } as cfg' -> cfg'

let handle_backspace_menu configs = List.map configs ~f:delete_value

let handle_backspace window =
  match window.current_state with
  | Menu -> { window with configs = handle_backspace_menu window.configs }
  | Typing ({ letters; _ } as typing) ->
      let letters = Letters.delete_last_current letters in
      { window with current_state = Typing { typing with letters } }
  | Summary _ -> window

let select_next_config configs =
  let selections (c : config) = c.selected in
  let shift lst =
    match List.rev lst with
    | ([] | [ _ ]) as r -> r
    | hd :: tl -> tl @ [ hd ] |> List.rev
  in
  List.map ~f:selections configs
  |> shift
  |> List.map2_exn ~f:(fun cfg sel -> { cfg with selected = sel }) configs

let handle_tab window =
  match window.current_state with
  | Menu -> { window with configs = select_next_config window.configs }
  | Typing _ -> window
  | Summary _ -> window

let handle_enter window =
  match window.current_state with
  | Menu -> { window with current_state = create_typing window }
  | Typing _ -> window
  | Summary _ -> window
