open Base

type t = (string, Words.t) Hashtbl.t

let create () : t = Hashtbl.create (module String)
