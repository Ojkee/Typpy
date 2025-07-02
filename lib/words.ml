(* TODO : open Base*)

type t = string array

let acceptable_word min max = function
  | word ->
      let len = String.length word in
      min <= len && len <= max

let file_content file_name =
  let ic = open_in file_name in
  let content = In_channel.input_all ic in
  close_in ic;
  content

let create ~file_name ~min ~max =
  file_content file_name |> String.split_on_char '\n' |> List.map String.trim
  |> List.filter (acceptable_word min max)
  |> Array.of_list

let length words = Array.length words
let get words idx = Array.get words idx
let to_list words = Array.to_list words
