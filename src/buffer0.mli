(* This exists to get rid of the circular dependency between Object and this type *)

type t = int 

(* Not deriving because we want to force people to use the Buffer.of_msgpack *)
val to_msgpack : t -> Msgpck.t
