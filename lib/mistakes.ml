open Base

type mistake = {
  inserted : char;
  target : char;
  prefix : char option;
  suffix : char option;
}

type t = mistake list
type mistake_with_count = (char * char) * int

let string_of_char (c : char) : string = String.make 1 c
let create () : t = []

let make_mistake ~(inserted : char) ~(target : char) ~(prefix : char option)
    ~(suffix : char option) : mistake =
  { inserted; target; prefix; suffix }

let length mistakes = List.length mistakes

let mistake_to_string_list ((i, t), count) =
  [ String.make 1 i; String.make 1 t; Int.to_string count ]

let prefix_ngram = function
  | { inserted = _; target = t; prefix = Some p; _ } ->
      Some (string_of_char p ^ string_of_char t)
  | { inserted = _; target = _; prefix = None; _ } -> None

let suffix_ngram = function
  | { inserted = _; target = t; prefix = _; suffix = Some s } ->
      Some (string_of_char t ^ string_of_char s)
  | { inserted = _; target = _; prefix = _; suffix = None } -> None

let add_mistake (mistakes : t) (m : mistake) : t = m :: mistakes

let common_counter (mistakes : t) : mistake_with_count list =
  let counter : (char * char, int) Hashtbl.Poly.t = Hashtbl.Poly.create () in
  let increase_counter key =
    match Hashtbl.find counter key with
    | None -> Hashtbl.set counter ~key ~data:1
    | Some count -> Hashtbl.set counter ~key ~data:(count + 1)
  in
  mistakes
  |> List.iter ~f:(fun { inserted = i; target = t; _ } ->
         increase_counter (i, t) );
  counter |> Hashtbl.to_alist

let common_counter_n ?(start = 0) ?(n = 5) (mistakes : t) :
    mistake_with_count list =
  let m =
    common_counter mistakes
    |> List.sort ~compare:(fun (_, c1) (_, c2) -> Int.compare c2 c1)
  in
  let start = Int.rem start (List.length m) in
  let len = min n (List.length m - start) |> fun x -> max 0 x in
  let start_len = n - len in
  let offset = m |> fun x -> List.sub x ~pos:start ~len in
  let wrapped = m |> fun x -> List.sub x ~pos:0 ~len:start_len in
  offset @ wrapped
