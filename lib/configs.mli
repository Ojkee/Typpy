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

val create : unit -> t
val get_int : t -> config_type -> int
val get_bool : t -> config_type -> bool
val insert_value : t -> char -> t
val delete_value : t -> t
val select_next : t -> t
val config_type_to_string : config_type -> string
val config_value_to_string : config_value -> string
val to_letters : t -> max_width:int -> Letters.t list
