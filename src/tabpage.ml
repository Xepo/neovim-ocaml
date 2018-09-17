include Window0

let of_msgpack (x:Msgpck.t) = 
  match Object.of_msgpack x with
  | Object.Tabpage x -> x
  | _ -> failwith "Expected tabpage"
;;
