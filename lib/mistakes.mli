type mistake = {
  inserted : char;
  target : char;
  prefix : char option;
  suffix : char option;
}

type t
type mistake_with_count

val create : unit -> t

val make :
  inserted:char ->
  target:char ->
  prefix:char option ->
  suffix:char option ->
  mistake

val length : t -> int
val mistake_to_string_list : mistake_with_count -> string list
val prefix_ngram : mistake -> string option
val suffix_ngram : mistake -> string option
val add : t -> mistake -> t
val common_counter : t -> mistake_with_count list
val common_counter_n : ?start:int -> ?n:int -> t -> mistake_with_count list
val add_if_happened : t -> Letters.t -> char -> t
val common_prefix_n : t -> n:int -> (string * int) list
val common_suffix_n : t -> n:int -> (string * int) list
val common_infix_n : t -> n:int -> (string * int) list
