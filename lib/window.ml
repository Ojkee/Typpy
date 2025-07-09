open Base

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
  mistake_start : int;
  mistake_n : int;
}

type state =
  | Menu
  | Typing of typing
  | Summary of summary

type lexicon = { words : Words.t (* memo : Lazy_table.t; *) }

type t = {
  current_state : state;
  lexicon : lexicon;
  configs : Configs.t;
}

let create_typing { lexicon = { words; _ }; configs; _ } =
  let n = Configs.get_int configs WordsNumber in
  let punctuation = Configs.get_bool configs Punctuation in
  let capitalize = Configs.get_bool configs Capitalize in
  let letters = Letters.init_n_as_letters ~words ~n ~punctuation ~capitalize in
  let mistakes = Mistakes.create () in
  Typing { letters; current_row = 0; mistakes; start_time = None }

let create () =
  let words = Words.create ~file_name:"data/words_alpha.txt" ~min:8 ~max:15 in
  (* let memo = Lazy_table.create () in *)
  let current_state = Menu in
  let configs = Configs.create () in
  { current_state; lexicon = { words (* ; memo *) }; configs }

let handle_menu_input_char { configs; _ } c = Configs.insert_value configs c

let handle_input_char window input : t =
  let update_state state = { window with current_state = state } in
  match window.current_state with
  | Menu -> { window with configs = handle_menu_input_char window input }
  | Typing ({ letters; mistakes; start_time; _ } as typing) -> (
      let letters = Letters.update letters input in
      let mistakes = Mistakes.add_if_happened mistakes letters input in
      let num_letters = Letters.lenght letters in
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
          let execution_time = Unix.gettimeofday () -. start in
          update_state
            (Summary
               {
                 mistakes;
                 num_letters;
                 execution_time;
                 mistake_start = 0;
                 mistake_n = 5;
               } )
      | true, None ->
          update_state
            (Summary
               {
                 mistakes;
                 num_letters;
                 execution_time = 0.;
                 mistake_start = 0;
                 mistake_n = 5;
               } ) )
  | Summary _ -> window

let handle_backspace_menu configs = Configs.delete_value configs

let handle_backspace window =
  match window.current_state with
  | Menu -> { window with configs = handle_backspace_menu window.configs }
  | Typing ({ letters; _ } as typing) ->
      let letters = Letters.delete_last_current letters in
      { window with current_state = Typing { typing with letters } }
  | Summary _ -> window

let handle_tab window =
  match window.current_state with
  | Menu -> { window with configs = Configs.select_next window.configs }
  | Typing _ -> window
  | Summary summary ->
      {
        window with
        current_state =
          Summary { summary with mistake_start = summary.mistake_start + 1 };
      }

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
