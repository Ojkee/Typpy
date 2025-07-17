open Base

type mistake = {
  inserted : char;
  target : char;
  prefix : char option;
  suffix : char option;
}

type t = mistake list
type mistake_with_count = (char * char) * int

let create () : t = []

let make ~inserted ~target ~prefix ~suffix =
  { inserted; target; prefix; suffix }

let length t = List.length t

let mistake_to_string_list ((i, t), count) =
  [ String.of_char i; String.of_char t; Int.to_string count ]

let prefix_ngram = function
  | { inserted = _; target = t; prefix = Some p; _ } ->
      Some (String.of_char p ^ String.of_char t)
  | { inserted = _; target = _; prefix = None; _ } -> None

let suffix_ngram = function
  | { inserted = _; target = t; prefix = _; suffix = Some s } ->
      Some (String.of_char t ^ String.of_char s)
  | { inserted = _; target = _; prefix = _; suffix = None } -> None

let add t mistake : t = mistake :: t

let common_counter t =
  let counter : (char * char, int) Hashtbl.Poly.t = Hashtbl.Poly.create () in
  let increase_counter key =
    match Hashtbl.find counter key with
    | None -> Hashtbl.set counter ~key ~data:1
    | Some count -> Hashtbl.set counter ~key ~data:(count + 1)
  in
  List.iter t ~f:(fun { inserted; target; _ } ->
      increase_counter (inserted, target) );
  counter |> Hashtbl.to_alist

let common_counter_n ?(start = 0) ?(n = 5) t =
  let m =
    common_counter t
    |> List.sort ~compare:(fun (_, c1) (_, c2) -> Int.compare c2 c1)
  in
  let start = Int.rem start (List.length m) in
  let len = min n (List.length m - start) |> fun x -> max 0 x in
  let start_len = n - len in
  let offset = m |> fun x -> List.sub x ~pos:start ~len in
  let wrapped = m |> fun x -> List.sub x ~pos:0 ~len:start_len in
  offset @ wrapped

let add_if_happened t letters input =
  let make ?prefix ?suffix target =
    make ~inserted:input ~target ~prefix ~suffix |> add t
  in
  let rec aux (lst : Letters.letter list) =
    match lst with
    | [] -> t
    | { c = _; status = Current } :: _ -> t
    | { c = target; status = Mistake } :: { c = after; status = Current } :: _
      ->
        make ~suffix:after target
    | { c = before; _ }
      :: { c = target; status = Mistake }
      :: { c = after; status = Current }
      :: _ ->
        make ~prefix:before ~suffix:after target
    | [ { c = before; _ }; { c = target; status = Mistake } ] ->
        make ~prefix:before target
    | [ { c = target; status = Mistake } ] -> make target
    | _ :: tl -> aux tl
  in
  aux (Letters.to_list letters)

let common_f_n t ~f ~n =
  List.filter_map t ~f
  |> List.sort ~compare:String.compare
  |> List.group ~break:String.( <> )
  |> List.map ~f:(fun x -> (List.hd_exn x, List.length x))
  |> List.sort ~compare:(fun (_, c1) (_, c2) -> Int.compare c2 c1)
  |> fun x -> List.take x n

let is_whitespace = function
  | ' '
  | '\n'
  | '\t'
  | '\r' ->
      true
  | _ -> false

let common_prefix_n t ~n =
  let f =
   fun { target; prefix; _ } ->
    match is_whitespace target with
    | true -> None
    | false -> Option.map prefix ~f:(fun p -> String.of_char_list [ p; target ])
  in
  common_f_n t ~f ~n

let common_suffix_n t ~n =
  let f =
   fun { target; suffix; _ } ->
    match is_whitespace target with
    | true -> None
    | false -> Option.map suffix ~f:(fun s -> String.of_char_list [ target; s ])
  in
  common_f_n t ~f ~n

let common_infix_n t ~n =
  let f =
   fun { target; prefix; suffix; _ } ->
    match is_whitespace target with
    | true -> None
    | false ->
        Option.map2 prefix suffix ~f:(fun p s ->
            String.of_char_list [ p; target; s ] )
  in
  common_f_n t ~f ~n
