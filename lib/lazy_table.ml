open Base

type t = (string, Words.t) Hashtbl.t

let create () : t = Hashtbl.create (module String)

let find_ngram_words t all_words ngram =
  Hashtbl.find_or_add t ngram ~default:(fun () ->
      Words.subset_of_substr all_words ~substring:ngram )
