open Base

type letter_status =
  | Current
  | Pending
  | Correct
  | Mistake

type t = {
  c : char;
  status : letter_status;
}

type color = {
  r : int;
  g : int;
  b : int;
}

type style = {
  fg : color;
  bg : color;
}

val bg_color : color
val fg_color : color
val status_style : letter_status -> style
val style_of_letter : t -> style
val init_n_as_letters : string array -> int -> t list
val next_space : t list -> int
val update_letters : t list -> char -> t list
val delete_last_current : t list -> t list
val print_letters : t list -> unit
