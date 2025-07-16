type t

val create : min:int -> max:int -> filename:string -> unit -> t
val random_n : t -> n:int -> t
val random_n_adaptive : t -> n:int -> mistakes:Mistakes.t -> t
val capitalize : t -> chance:float -> t
val punctuate : t -> chance:float -> t
val to_letters : t -> Letters.t
