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

val create_typing : words:string array -> n:int -> t
val input_update : t -> char -> string array -> int -> t
val backspace_update : t -> t
