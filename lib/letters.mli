open Base

type letter_status =
  | Current
  | Pending
  | Correct
  | Mistake

type t = {
  c : char;
  mutable status : letter_status;
}

type color = {
  r : int;
  g : int;
  b : int;
}

type style = {
  fg : color option;
  bg : color option;
}

val bg_color : color
val fg_color : color
val status_style : letter_status -> style
val string_of_status : letter_status -> string
val style_of_letter : t -> style
val take_n_as_letters : string array -> int -> t list
val next_space : t list -> int
val update_letters : t list -> char -> t list
val delete_last_current : t list -> t list
val print_letters : t list -> unit
