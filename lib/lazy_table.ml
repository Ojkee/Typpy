module LazyTable = struct
  type t = (string, string list) Hashtbl.t

  let create () : t = Hashtbl.create 8

  let contains (word : string) (ngram : string) : bool =
    let re = Str.regexp_string ngram in
    try
      ignore (Str.search_forward re word 0);
      true
    with
    | Not_found -> false

  let add_words_with_ngram (table : t) (words : string array) (ngram : string) :
      unit =
    match Hashtbl.find_opt table ngram with
    | None ->
        let rec aux acc = function
          | [] -> acc
          | hd :: tl when contains hd ngram -> aux (hd :: acc) tl
          | _ :: tl -> aux acc tl
        in
        Hashtbl.add table ngram (aux [] (Array.to_list words))
    | Some _ -> ()

  let from_ngram (table : t) (ngram : string) : string list option =
    Hashtbl.find_opt table ngram

  let take_random_n (n : int) (maxn : int) : int list =
    Random.self_init ();
    let rec aux acc = function
      | 0 -> acc
      | k -> aux (Random.int maxn :: acc) (k - 1)
    in
    aux [] n

  let rec random_n_from_ngram (table : t) (words : string array)
      (ngram : string) (n : int) : string list =
    match from_ngram table ngram with
    | None ->
        add_words_with_ngram table words ngram;
        random_n_from_ngram table words ngram n
    | Some lst when List.length lst <= n -> lst
    | Some lst ->
        let rand_n = take_random_n n (List.length lst) in
        let find_idx i _ = List.mem i rand_n in
        lst |> List.filteri find_idx

  let random_n_words (words : string array) (n : int) : string list =
    let rand_n = take_random_n n (Array.length words) in
    let rec aux acc = function
      | [] -> acc
      | hd :: tl -> aux (Array.get words hd :: acc) tl
    in
    aux [] rand_n
end
