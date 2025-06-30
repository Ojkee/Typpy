type typing = {
  letters : Letters.t;
  mistakes : Mistakes.t;
}

type summary = { mistakes : Mistakes.t }

type t =
  | Typing of typing
  | Summary of summary

val create_typing : words:string array -> n:int -> t
val input_update : t -> char -> t
val backspace_update : t -> t
