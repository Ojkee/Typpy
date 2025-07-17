open Base

type t = (string, Words.t) Hashtbl.t

val create : unit -> t
val find_ngram_words : t -> Words.t -> string -> Words.t
