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
val append : t -> t -> t
val rev : t -> t
val lenght : t -> int
val of_list : letter list -> t
val to_list : t -> letter list
val of_string : ?status:letter_status -> string -> t
val status_style : letter_status -> style
val style_of_letter : letter -> style

val init_n_as_letters :
  words:Words.t -> n:int -> punctuation:bool -> capitalize:bool -> t

val set_current_n : t -> n:int -> t
val init_from_list : string list -> t
val next_space : t -> int
val to_rows : t -> max_width:int -> t list
val update : t -> char -> t
val delete_last_current : t -> t
val correct_count : t -> int
val finished : t -> bool
val exists : t -> f:(letter -> bool) -> bool
val current_row_idx : t list -> int
val words_till_current : t -> int
val is_current_f : t -> f:(letter -> bool) -> bool
val is_next_f : t -> f:(letter -> bool) -> bool
val is_space : letter -> bool
val words_left : t -> int
val remove_words_before_n_current : t -> n:int -> t
