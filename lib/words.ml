open Base

type t = string array

let acceptable_word min max = function
  | word ->
      let len = String.length word in
      min <= len && len <= max

let file_content filename =
  let ic = Stdio.In_channel.create filename in
  let content = In_channel.input_all ic in
  Stdio.In_channel.close ic;
  content

let to_list words = Array.to_list words
let of_list lst = Array.of_list lst

let create ~min ~max ~filename =
  file_content filename
  |> String.split_on_chars ~on:[ '\n' ]
  |> List.map ~f:String.strip
  |> List.filter ~f:(acceptable_word min max)
  |> of_list

let length words = Array.length words
let get words idx = Array.get words idx

let is_rand_above_threshold ~chance =
  assert (Float.( <= ) 0. chance && Float.( <= ) chance 1.);
  Random.self_init ();
  let r = Random.float 1. in
  Float.( >= ) r chance

let get_random_punctuation () =
  let p =
    [|
      "!";
      "\"";
      "#";
      "$";
      "%";
      "&";
      "'";
      "*";
      "+";
      ",";
      "-";
      ".";
      "/";
      ":";
      ";";
      "<";
      "=";
      ">";
      "?";
      "[";
      "\\";
      "^";
      "_";
      "`";
      "{";
      "|";
      "~";
    |]
  in
  Random.self_init ();
  let r = Random.int (Array.length p) in
  p.(r)

let punctuate t ~chance =
  let aux word =
    if is_rand_above_threshold ~chance then word
    else
      match get_random_punctuation () with
      | ("\'" | "\"") as q -> q ^ word ^ q
      | "(" -> "(" ^ word ^ ")"
      | "{" -> "{" ^ word ^ "}"
      | "[" -> "[" ^ word ^ "]"
      | ("`" | "~" | "#") as prefix -> prefix ^ word
      | (">" | "<" | "+" | "*" | "\\") as infix -> word ^ " " ^ infix
      | p -> word ^ p
  in
  Array.map t ~f:aux

let capitalize t ~chance =
  let aux word =
    if is_rand_above_threshold ~chance then word else String.capitalize word
  in
  Array.map t ~f:aux

let take_random_n (n : int) (maxn : int) : int list =
  Random.self_init ();
  let rec aux acc = function
    | 0 -> acc
    | k -> aux (Random.int maxn :: acc) (k - 1)
  in
  aux [] n

let random_n (t : t) ~n =
  let rand_n = take_random_n n (length t) in
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (get t hd :: acc) tl
  in
  aux [] rand_n |> of_list

let subset_of_substr t ~substring =
  Array.filter t ~f:(fun word -> String.is_substring word ~substring)

let concat t_list = Array.concat t_list
