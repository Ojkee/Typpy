open Base

type typing = {
  letters : Letters.t;
  mistakes : Mistakes.t;
  start_time : float option;
  inputs_count : int;
  word_count : int;
}

type summary = {
  letters : Letters.t;
  mistakes : Mistakes.t;
  inputs_count : int;
  execution_time : float;
  mistake_start : int;
  mistake_n : int;
}

type state =
  | Menu
  | Typing of typing
  | Summary of summary

type lexicon = { words : Words.t (* memo : Lazy_table.t; *) }

type t = {
  cols : int;
  rows : int;
  current_state : state;
  lexicon : lexicon;
  configs : Configs.t;
}

let punctuation_chance configs =
  Configs.get_bool configs Punctuation |> Result.ok_or_failwith |> fun p ->
  match p with
  | true -> 0.2
  | false -> 0.0

let capitalize_chance configs =
  Configs.get_bool configs Capitalize |> Result.ok_or_failwith |> fun c ->
  match c with
  | true -> 0.2
  | false -> 0.0

let letters_n configs =
  Configs.get_int_type configs WordsNumber |> Result.ok_or_failwith
  |> fun int_type ->
  match int_type with
  | Configs.Finite x -> Int.of_string x
  | Infinite -> 100

let generate_new_words words configs =
  Words.random_n words ~n:(letters_n configs)
  |> Words.punctuate ~chance:(punctuation_chance configs)
  |> Words.capitalize ~chance:(capitalize_chance configs)

let create_typing { lexicon = { words; _ }; configs; _ } =
  let letters =
    generate_new_words words configs
    |> Letters.of_words |> Letters.set_current_n ~n:0
  in
  let mistakes = Mistakes.create () in
  Typing
    { letters; mistakes; start_time = None; inputs_count = 0; word_count = 0 }

let create ~cols ~rows () =
  let words = Words.create ~file_name:"data/words_alpha.txt" ~min:8 ~max:15 in
  (* let memo = Lazy_table.create () in *)
  let current_state = Menu in
  let configs = Configs.create () in
  { cols; rows; current_state; lexicon = { words (* ; memo *) }; configs }

let to_summary ?et letters mistakes inputs_count =
  let execution_time = et |> Option.value ~default:0.0 in
  Summary
    {
      letters;
      mistakes;
      inputs_count;
      execution_time;
      mistake_start = 0;
      mistake_n = 5;
    }

let update_word_count_after_input letters word_count =
  match Letters.is_current_f letters ~f:Letters.is_space with
  | true -> word_count + 1
  | false -> word_count

let update_word_count_after_backspace letters word_count =
  match Letters.is_next_f letters ~f:Letters.is_space with
  | true -> word_count - 1
  | false -> word_count

let is_infinite configs =
  match Configs.(get_int_type configs WordsNumber) with
  | Ok Infinite -> true
  | _ -> false

let update_letters t letters input_char =
  let letters = Letters.update letters input_char in
  match (is_infinite t.configs, Letters.words_left letters < 50) with
  | true, true ->
      let new_letters =
        generate_new_words t.lexicon.words t.configs |> Letters.of_words
      in
      Letters.append letters new_letters
  | _, _ -> letters

let handle_input_char t input_char : t =
  let update_state state = { t with current_state = state } in
  match t.current_state with
  | Menu -> { t with configs = Configs.insert_value t.configs input_char }
  | Typing
      ({ letters; mistakes; start_time; inputs_count; word_count; _ } as typing)
    -> (
      let letters = update_letters t letters input_char in
      let word_count = update_word_count_after_input letters word_count in
      let mistakes = Mistakes.add_if_happened mistakes letters input_char in
      let inputs_count = inputs_count + 1 in
      let update_typing ?start_time () =
        Typing { letters; mistakes; inputs_count; start_time; word_count }
      in
      match (Letters.finished letters, start_time) with
      | false, None ->
          update_state (update_typing ~start_time:(Unix.gettimeofday ()) ())
      | false, _ ->
          let start_time = typing.start_time |> Option.value ~default:0. in
          update_state (update_typing ~start_time ())
      | true, Some start ->
          let et = Unix.gettimeofday () -. start in
          update_state (to_summary ~et letters mistakes inputs_count)
      | true, None -> update_state (to_summary letters mistakes inputs_count) )
  | Summary _ -> t

let handle_backspace t =
  match t.current_state with
  | Menu -> { t with configs = Configs.delete_value t.configs }
  | Typing ({ letters; inputs_count; word_count; _ } as typing) ->
      let letters = Letters.delete_last_current letters in
      let word_count = update_word_count_after_backspace letters word_count in
      let inputs_count = inputs_count + 1 in
      {
        t with
        current_state = Typing { typing with letters; inputs_count; word_count };
      }
  | Summary _ -> t

let handle_tab t =
  match t.current_state with
  | Menu -> { t with configs = Configs.select_next t.configs }
  | Typing _ -> t
  | Summary summary ->
      let mistake_start = summary.mistake_start + 1 in
      { t with current_state = Summary { summary with mistake_start } }

let handle_enter t =
  match t.current_state with
  | Menu -> { t with current_state = create_typing t }
  | Typing _ -> t
  | Summary _ -> { t with current_state = create_typing t }

let handle_esc t =
  match t.current_state with
  | Menu -> None
  | Typing _ -> Some { t with current_state = Menu }
  | Summary _ -> Some { t with current_state = Menu }
