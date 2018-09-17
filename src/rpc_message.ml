open! Core
open! Async
open Protocol_conv_msgpack
module M2 = Tcp

module On_the_wire = struct
  type 'a t = (int * int * 'a * Msgpack.t) [@@deriving protocol ~driver:(module Msgpack)]
end

type 'a t = 
  { request : int
  ; id : int
  ; name : 'a
  ; payload : Msgpck.t
  }

let of_msgpack_unit x = 
  let (request, id, name, payload) = On_the_wire.of_msgpack (fun _ -> ()) x in
  {request; id; name ; payload}
;;

let to_msgpack {request; id; name; payload} = 
  On_the_wire.to_msgpack Msgpck.of_string (request, id, name, payload)
;;
