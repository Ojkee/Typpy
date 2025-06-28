module FileUtils = struct
  let acceptable_word min max = function
    | word ->
        let len = String.length word in
        min <= len && len <= max

  let file_content file_name =
    let ic = open_in file_name in
    let content = In_channel.input_all ic in
    close_in ic;
    content

  let content_words content w_min w_max =
    let words =
      content |> String.split_on_char '\n' |> List.map String.trim
      |> List.filter (acceptable_word w_min w_max)
    in
    words |> Array.of_list
end
