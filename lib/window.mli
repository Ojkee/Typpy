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

type lexicon

type t = {
  current_state : state;
  lexicon : lexicon;
  configs : configs;
}

val config_type_to_string : config_type -> string
val config_value_to_string : config_value -> string
val create_typing : t -> state
val create : unit -> t
val handle_input_char : t -> char -> t
val handle_backspace : t -> t
val handle_tab : t -> t
val handle_enter : t -> t
