open! Core
open! Protocol_conv_msgpack

type t = | Buffer of Buffer0.t | Window of Window0.t | Tabpage of Tabpage0.t

let ext_types = ref Int.Map.empty
let (ext_types' : ([`Buffer | `Tabpage | `Window], int) Map.Poly.t ref) = ref Map.Poly.empty

let set_ext_types x = 
  let data =
    List.map x ~f:(fun (id, name) -> 
        id, 
        match name with
        | "Buffer" -> `Buffer 
        | "Window" -> `Window 
        | "Tabpage" -> `Tabpage 
        | x -> failwithf "Unknown type %s" x ()
      )
  in
  ext_types := Int.Map.of_alist_exn data;
  ext_types' := Map.Poly.of_alist_exn (List.map ~f:(fun (x,y) -> y,x) data);
;;

let of_msgpack = function
  | Msgpck.Ext (id, x) -> 
    let x = Msgpck.to_int (Msgpck.String.read x |> snd) in
    (match Map.find_exn !ext_types id with
    | `Buffer -> Buffer x
    | `Window -> Window x
    | `Tabpage -> Tabpage x)
  | _ -> failwith "Invalid type"
;;

let to_msgpack x =
  let (ext_type, data) = 
    match x with
    | Buffer b -> Map.find_exn (!ext_types') `Buffer, b
    | Window b -> Map.find_exn (!ext_types') `Window, b
    | Tabpage b -> Map.find_exn (!ext_types') `Tabpage, b
  in
  Msgpck.Ext (ext_type, Msgpck.String.to_string (Msgpck.of_int data))
;;


