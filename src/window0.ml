open Protocol_conv_msgpack

type t = int [@@deriving protocol ~driver:(module Msgpack)]
