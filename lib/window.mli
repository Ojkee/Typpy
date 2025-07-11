type typing = {
  letters : Letters.t;
  current_row : int;
  mistakes : Mistakes.t;
  start_time : float option;
  inputs_count : int;
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

type lexicon

type t = {
  current_state : state;
  lexicon : lexicon;
  configs : Configs.t;
}

val create_typing : t -> state
val create : unit -> t
val handle_input_char : t -> char -> t
val handle_backspace : t -> t
val handle_tab : t -> t
val handle_enter : t -> t
val handle_esc : t -> t option
