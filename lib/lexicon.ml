open Base

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
  ignore t.ngrams_memo;
  if Mistakes.length mistakes = 0 then random_n t ~n
  else
    let pre = Mistakes.common_prefix_n mistakes ~n:10 in
    let inf = Mistakes.common_infix_n mistakes ~n:10 in
    let suf = Mistakes.common_suffix_n mistakes ~n:10 in
    let fst (p, _) = p in
    let fsts lst = List.map lst ~f:fst in
    let bigrams =
      [ pre; suf ] |> List.concat_map ~f:fsts
      |> List.dedup_and_sort ~compare:String.compare
    in
    let infix_score substring =
      List.filter_map inf ~f:(fun (infix, c) ->
          if String.is_substring infix ~substring then Some c else None )
      |> List.fold ~init:0 ~f:( + )
    in
    let bigram_score bigram alst =
      List.Assoc.find alst ~equal:String.equal bigram |> Option.value ~default:0
    in
    let score bigram =
      bigram_score bigram pre + bigram_score bigram suf + infix_score bigram
    in
    let most_common =
      List.map bigrams ~f:(fun b -> (b, score b))
      |> List.sort ~compare:(fun (_, s1) (_, s2) -> Int.compare s2 s1)
      |> List.map ~f:(fun (s, _) -> s)
      |> fun common -> List.take common 3
    in
    ignore most_common;
    failwith "TODO"

let capitalize t ~chance = { t with words = Words.capitalize t.words ~chance }
let punctuate t ~chance = { t with words = Words.punctuate t.words ~chance }
let to_letters t = Letters.of_words t.words
