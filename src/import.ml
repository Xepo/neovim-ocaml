open Protocol_conv_msgpack

(* This file gets pulled into the autogenerated ml files.  This provides some msgpack converters that it expects. *)
type 'a tuple_2 = 'a * 'a  [@@deriving protocol ~driver:(module Msgpack)]
type nonrec 'a list = 'a list [@@deriving protocol ~driver:(module Msgpack)]
