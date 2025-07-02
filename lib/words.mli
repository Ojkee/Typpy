type t

val create : file_name:string -> min:int -> max:int -> t
val length : t -> int
val get : t -> int -> string
val to_list : t -> string list
