type t

val to_list : t -> string list
val of_list : string list -> t
val create : min:int -> max:int -> filename:string -> t
val length : t -> int
val get : t -> int -> string
val punctuate : t -> chance:float -> t
val capitalize : t -> chance:float -> t
val random_n : t -> n:int -> t
