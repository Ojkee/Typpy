type mode =
  | Typing of Letters.t list * Mistakes.t
  | Summary of Mistakes.t

type state = { mode : mode }
