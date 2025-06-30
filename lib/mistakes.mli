type mistake = {
  inserted : char;
  target : char;
  prefix : char option;
  suffix : char option;
}

type t

val create : unit -> t
val prefix_ngram : mistake -> string option
val suffix_ngram : mistake -> string option
val add_mistake : t -> mistake -> t
val common_counter : t -> ((char * char) * int) list
val common_counter_top_n : t -> int -> ((char * char) * int) list
(* val n_common_prefix : t -> int -> t *)
(* val n_common_suffix : t -> int -> t *)
