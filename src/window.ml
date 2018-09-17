open Protocol_conv_msgpack
type t = int [@@deriving protocol ~driver:(module Msgpack)]

let of_msgpack (x:Msgpck.t) = 
  match Object.of_msgpack x with
  | Object.Window x -> x
  | _ -> failwith "Expected window"
;;
