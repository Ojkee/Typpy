type mistake = {
  inserted : char;
  target : char;
  prefix : char option;
  suffix : char option;
}

type t
type mistake_with_count

val create : unit -> t

val make_mistake :
  inserted:char ->
  target:char ->
  prefix:char option ->
  suffix:char option ->
  mistake

val mistake_to_string_list : mistake_with_count -> string list
val prefix_ngram : mistake -> string option
val suffix_ngram : mistake -> string option
val add_mistake : t -> mistake -> t
val common_counter : t -> mistake_with_count list
val common_counter_top_n : t -> int -> mistake_with_count list
(* val n_common_prefix : t -> int -> t *)
(* val n_common_suffix : t -> int -> t *)
