open Protocol_conv_msgpack

type 'a tuple_2 = 'a * 'a  [@@deriving protocol ~driver:(module Msgpack)]
type nonrec 'a list = 'a list [@@deriving protocol ~driver:(module Msgpack)]
