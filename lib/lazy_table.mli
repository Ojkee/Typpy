type t = (string, string list) Hashtbl.t

val create : unit -> t
val add_words_with_ngram : t -> string array -> string -> unit
val from_ngram : t -> string -> string list option
val random_n_from_ngram : t -> string array -> string -> int -> string list
val random_n_words : string array -> int -> string list
