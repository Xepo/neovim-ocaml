include Buffer0

let of_msgpack (x:Msgpck.t) = 
  match Object.of_msgpack x with
  | Object.Buffer x -> x
  | _ -> failwith "Expected buffer"
;;
