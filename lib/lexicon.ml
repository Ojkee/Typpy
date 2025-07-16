type t = {
  words : Words.t;
  all_words : Words.t;
  ngrams_memo : Lazy_table.t;
}

let create ~min ~max ~filename () =
  let words = Words.of_list []
  and all_words = Words.create ~min ~max ~filename
  and ngrams_memo = Lazy_table.create () in
  { words; all_words; ngrams_memo }

let random_n t ~n = { t with words = Words.random_n t.all_words ~n }

let random_n_adaptive t ~n ~mistakes =
  let mistakes_count = Mistakes.common_counter_n mistakes in
  ignore (t, n, mistakes, t.ngrams_memo, mistakes_count);
  failwith "TODO"

let capitalize t ~chance = { t with words = Words.capitalize t.words ~chance }
let punctuate t ~chance = { t with words = Words.punctuate t.words ~chance }
let to_letters t = Letters.of_words t.words
