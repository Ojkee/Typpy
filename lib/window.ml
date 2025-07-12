open Base

type typing = {
  letters : Letters.t;
  current_row : int;
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

let create_typing { lexicon = { words; _ }; configs; _ } =
  let n = Configs.get_int_type configs WordsNumber |> Result.ok_or_failwith in
  let punctuation =
    Configs.get_bool configs Punctuation |> Result.ok_or_failwith
  in
  let capitalize =
    Configs.get_bool configs Capitalize |> Result.ok_or_failwith
  in
  let letters =
    ( match n with
    | Configs.Finite x -> Int.of_string x
    | Infinite -> 100 )
    |> fun n ->
    Letters.init_n_as_letters ~words ~n ~punctuation ~capitalize
    |> Letters.set_current_n ~n:0
  in
  let mistakes = Mistakes.create () in
  Typing
    {
      letters;
      current_row = 0;
      mistakes;
      start_time = None;
      inputs_count = 0;
      word_count = 0;
    }

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

let update_letters letters window input_char =
  let letters = Letters.update letters input_char in
  match (is_infinite window.configs, Letters.words_left letters < 50) with
  | true, true ->
      let new_words = Lazy_table.random_n_words window.lexicon.words 50 in
      let new_letters = Letters.init_from_list new_words in
      letters |> Letters.append new_letters
  | _, _ -> letters

let handle_input_char window input_char : t =
  let update_state state = { window with current_state = state } in
  match window.current_state with
  | Menu ->
      { window with configs = Configs.insert_value window.configs input_char }
  | Typing
      ({ letters; mistakes; start_time; inputs_count; word_count; _ } as typing)
    -> (
      let letters = update_letters letters window input_char in
      let word_count = update_word_count_after_input letters word_count in
      let mistakes = Mistakes.add_if_happened mistakes letters input_char in
      let inputs_count = inputs_count + 1 in
      let update_typing ?start_time typing' =
        Typing
          {
            typing' with
            letters;
            mistakes;
            inputs_count;
            start_time;
            word_count;
          }
      in
      match (Letters.finished letters, start_time) with
      | false, None ->
          update_state (update_typing typing ~start_time:(Unix.gettimeofday ()))
      | false, _ ->
          let start_time = typing.start_time |> Option.value ~default:0. in
          update_state (update_typing typing ~start_time)
      | true, Some start ->
          let et = Unix.gettimeofday () -. start in
          update_state (to_summary ~et letters mistakes inputs_count)
      | true, None -> update_state (to_summary letters mistakes inputs_count) )
  | Summary _ -> window

let handle_backspace window =
  match window.current_state with
  | Menu -> { window with configs = Configs.delete_value window.configs }
  | Typing ({ letters; inputs_count; word_count; _ } as typing) ->
      let letters = Letters.delete_last_current letters in
      let word_count = update_word_count_after_backspace letters word_count in
      let inputs_count = inputs_count + 1 in
      {
        window with
        current_state = Typing { typing with letters; inputs_count; word_count };
      }
  | Summary _ -> window

let handle_tab window =
  match window.current_state with
  | Menu -> { window with configs = Configs.select_next window.configs }
  | Typing _ -> window
  | Summary summary ->
      let mistake_start = summary.mistake_start + 1 in
      { window with current_state = Summary { summary with mistake_start } }

let handle_enter window =
  match window.current_state with
  | Menu -> { window with current_state = create_typing window }
  | Typing _ -> window
  | Summary _ -> { window with current_state = create_typing window }

let handle_esc window =
  match window.current_state with
  | Menu -> None
  | Typing _ -> Some { window with current_state = Menu }
  | Summary _ -> Some { window with current_state = Menu }
