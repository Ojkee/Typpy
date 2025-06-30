type typing = {
  letters : Letters.t;
  mistakes : Mistakes.t;
  start_time : float option;
}

type summary = {
  mistakes : Mistakes.t;
  num_letters : int;
  execution_time : float;
}

type t =
  | Typing of typing
  | Summary of summary

let create_typing ~words ~n : t =
  let letters = Letters.init_n_as_letters words n in
  let mistakes = Mistakes.create () in
  Typing { letters; mistakes; start_time = None }

let mistake_if_happened (letters : Letters.t) (mistakes : Mistakes.t)
    (input : char) : Mistakes.t =
  let make ?prefix ?suffix target =
    Mistakes.make_mistake ~inserted:input ~target ~prefix ~suffix
    |> Mistakes.add_mistake mistakes
  in
  let rec aux (lst : Letters.letter list) =
    match lst with
    | [] -> mistakes
    | { c = _; status = Current } :: _ -> mistakes
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

let input_update (state : t) (input : char) (words : string array) (n : int) : t
    =
  match state with
  | Typing { letters; mistakes; start_time; _ } -> (
      let letters = Letters.update_letters letters input in
      let mistakes = mistake_if_happened letters mistakes input in
      match (Letters.finished letters, start_time) with
      | false, None ->
          Typing { letters; mistakes; start_time = Some (Unix.gettimeofday ()) }
      | false, _ -> Typing { letters; mistakes; start_time }
      | true, Some start ->
          let execution_time = Unix.gettimeofday () -. start in
          let num_letters = Letters.lenght letters in
          Summary { mistakes; num_letters; execution_time }
      | true, None ->
          let num_letters = Letters.lenght letters in
          Summary { mistakes; num_letters; execution_time = 0. } )
  | Summary _ as s -> if input = 'r' then create_typing ~words ~n else s

let backspace_update (state : t) : t =
  match state with
  | Typing { letters; mistakes; start_time } ->
      let letters = Letters.delete_last_current letters in
      Typing { letters; mistakes; start_time }
  | Summary _ as s -> s
