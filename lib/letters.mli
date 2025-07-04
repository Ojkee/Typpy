open Base

type letter_status =
  | Current
  | Pending
  | Correct
  | Mistake
  | Text
  | SelectedText
  | SummaryTable

type letter = {
  c : char;
  status : letter_status;
}

type t

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
val create : unit -> t
val rev : t -> t
val lenght : t -> int
val of_list : letter list -> t
val to_list : t -> letter list
val of_string : ?status:letter_status -> string -> t
val status_style : letter_status -> style
val style_of_letter : letter -> style
val init_n_as_letters : Words.t -> int -> t
val next_space : t -> int
val to_rows : t -> int -> t list
val update : t -> char -> t
val delete_last_current : t -> t
val finished : t -> bool
val exists : t -> f:(letter -> bool) -> bool
