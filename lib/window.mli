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

val config_value_to_string : config_value -> string
val create_menu : unit -> t
val create_typing : words:string array -> n:int -> t
val handle_input_char : t -> char -> string array -> int -> t
val handle_backspace : t -> t
val handle_tab : t -> t
val handle_enter : t -> t
