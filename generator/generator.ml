open Core
open Async
open Protocol_conv_msgpack
module M2 = Tcp

let indent s =
  "\t" ^ String.substr_replace_all s ~pattern:"\n" ~with_:"\n\t"

module String_map_msgpack = struct
  type 'a t = 'a String.Map.t [@@deriving sexp]

  let to_msgpack f t = 
    Msgpck.Map (
      Map.to_alist t
      |> List.map ~f:(fun (s,x) -> 
          Msgpck.String s, f x))

  let of_msgpack f t = 
    match t with
    | Msgpck.Map l -> 
      List.map l ~f:(function (String s, x) -> (s, f x) | _ -> failwith "Expected Msgpck.String Map but got other type")
      |> String.Map.of_alist_exn
    | _ -> failwith "Expected Msgpck.Map but got other type"
  ;;
end

module Protocol = struct
  module Datatype = struct
    module Base = struct
      type t = 
        | Basetype of string
        | Nvimtype of string
        | List of t
        | Tuple of t * int

      let rec of_string = function
        | "Integer" -> Basetype "int"
        | "Boolean" -> Basetype "bool"
        | "void" -> Basetype "unit"
        | "String" -> Basetype "string"
        | "Array" -> List (Basetype "string")
        | "Buffer" -> Nvimtype "Buffer"
        | "Window" -> Nvimtype "Window"
        | "Tabpage" -> Nvimtype "Tabpage"
        | "Object" -> Nvimtype "Object"
        | "Dictionary" -> Nvimtype "Dictionary"
        | x -> 
          match String.chop_prefix x ~prefix:"ArrayOf(" with
          | None -> failwithf "Don't recognize type %s" x ()
          | Some x -> 
            let x = String.drop_suffix x 1 in
            match String.split ~on:',' x with
            | [x] -> List (of_string x)
            | [x;n] -> 
              Tuple (of_string x, Int.of_string (String.strip n))
            | _ -> failwithf "Don't recognize type %s" x ()
      ;;

      let rec to_mli = function
        | Basetype x -> x
        | Nvimtype x -> x ^ ".t"
        | List x -> (to_mli x) ^ " list"
        | Tuple (x, amt) -> 
          "(" ^ 
          (List.init amt ~f:(const ( to_mli x))
          |> String.concat ~sep:" * ")
            ^ ")"
      ;;

      let get_module = function
        | Nvimtype x -> x
        | _ -> failwith "No such module"
      ;;

      let rec msgpack ~direction t = 
        let suffix = match direction with | `To_msgpack -> "to_msgpack" | `From_msgpack -> "of_msgpack" in
        match t with
        | Basetype "unit" -> (match direction with |`From_msgpack -> "(fun _ -> ())" | `To_msgpack -> failwith "How do you read unit?")
        | Basetype x -> 
          let for_base = match direction with | `To_msgpack -> "of" | `From_msgpack -> "to" in
          sprintf "Msgpck.%s_%s" for_base x
        | Nvimtype x -> x ^ "." ^ suffix
        | List x -> sprintf "(list_%s %s)" suffix (msgpack ~direction x)
        | Tuple (x, amt) ->  sprintf "(tuple_%d_%s %s)" amt suffix (msgpack ~direction x)
      ;;

      let of_msgpack = msgpack ~direction:`From_msgpack
      let to_msgpack = msgpack ~direction:`To_msgpack
 
    end
    type t = string

    let get_module t = Base.get_module (Base.of_string t)

    let to_mli t = (Base.to_mli (Base.of_string t))

    let msgpack_base = function
      | "Array" -> "string_list_"
      | dt -> 
        match String.chop_suffix ~suffix:".t" (to_mli dt) with
        | Some prefix -> prefix ^ "."
        | None -> to_mli dt ^ "_"
    ;;

    let of_msgpack t = Base.of_msgpack (Base.of_string t)
    let to_msgpack t = Base.to_msgpack (Base.of_string t)
  end

  module Parameters = struct
    type t = (Datatype.t * string) list

    let ocaml_name = function
      | "end" -> "end_"
      | name -> name

    let should_be_named_parameter ~ocaml_datatype ~name = 
      match ocaml_datatype, name with
      | "str", "string" -> false
      | _ -> 
        not (String.lowercase ocaml_datatype = name || String.lowercase ocaml_datatype = (name ^ ".t"))
    ;;

    let to_mli ~is_method t =
      let (fst, rest) =
        if not is_method 
        then [], t
        else
          match t with
          | (_, _) :: rest -> ["t"], rest
          | [] -> failwith "Method but no parameters?"
      in
      let rest = 
        List.map rest ~f:(fun (datatype, name) -> 
            let ocaml_datatype = Datatype.to_mli datatype in
            let named_param = 
              if should_be_named_parameter ~ocaml_datatype ~name
              then ocaml_name name ^ ":"
              else ""
            in
            sprintf !"%s%s" named_param ocaml_datatype)
      in
      String.concat ~sep:" -> " (fst @ rest)
    ;;

    let to_ml_params ~is_method (t:t) = 
      let (fst, rest) =
        if not is_method 
        then [], t
        else
          match t with
          | (_, name) :: rest -> [name], rest
          | [] -> failwith "Method but no parameters?"
      in
      let rest = 
        List.map rest ~f:(fun (datatype, name) -> 
            let ocaml_datatype = Datatype.to_mli datatype in
            if should_be_named_parameter ~ocaml_datatype ~name
            then "~" ^ ocaml_name name
            else ocaml_name name)
      in
      String.concat ~sep:" " (fst @ rest)
    ;;

    let to_msgpack t =
      sprintf "(Msgpck.of_list [%s])"
        (List.map t ~f:(fun (datatype, name) -> 
             Datatype.to_msgpack datatype ^ " " ^ ocaml_name name)
         |> String.concat ~sep:";")
    ;;
  end

  module Error_type = struct
    type t = { id : int } [@@deriving protocol ~driver:(module Msgpack), sexp]
  end
  
  module Func = struct
    type t = {
      is_method : bool [@key "method"];
      name : string;
      parameters : (string * string) list;
      return_type : string;
      since : int;
      deprecated_since : int option
    } [@@deriving protocol ~driver:(module Msgpack), sexp]



    let default_deprecated_since = (Msgpck.of_string "deprecated_since", Msgpck.of_nil)

    let of_msgpack msgpck =
      try 
        of_msgpack msgpck
      with
      | exn -> 
        match msgpck with
        | Msgpck.Map s -> 
          let new_s = default_deprecated_since :: s in
          of_msgpack (Msgpck.Map new_s)
        | _ -> Exn.reraise exn "Not a map?"
  ;;


    let which_module t = 
      if t.is_method 
      then Some (List.hd_exn t.parameters |> fst |> Datatype.get_module)
      else None
    ;;

    let module_prefix = 
      String.Map.of_alist_exn
        [ "Window", "win_"
        ; "Buffer", "buf_"
        ; "Tabpage", "tabpage_"
        ]

    let ocaml_name t = 
      let remove_prefix prefix s = 
        String.chop_prefix ~prefix s
        |> Option.value ~default:s 
      in
      let module_prefix =
        match which_module t with
        | None -> ""
        | Some module_ -> 
          Option.value ~default:"" (Map.find module_prefix module_)
      in
      let simplified = 
        t.name
        |> remove_prefix "nvim_"
        |> remove_prefix module_prefix
      in
      match t.deprecated_since with
      | None -> simplified
      | Some x -> sprintf "_deprecated_v%d_%s" x simplified
    ;;

    let to_mli ({is_method; name=_; parameters; return_type; since=_; deprecated_since=_} as t) =
      let arrow_after_conn = match parameters with | [] -> "" | _ -> " -> " in
      sprintf !"val %{ocaml_name} : conn:Conn.t%s%s -> %{Datatype.to_mli} Conn.result" t arrow_after_conn (Parameters.to_mli ~is_method parameters) return_type
    ;;

    let to_ml ({is_method; parameters; _} as t) =
      sprintf !"let %{ocaml_name} ~conn %s =\n" t (Parameters.to_ml_params ~is_method parameters)
        ^ indent (sprintf "Conn.call conn \"%s\" %s %s\n" t.name (Parameters.to_msgpack t.parameters) (Datatype.of_msgpack t.return_type))
  end
  
  module Version = struct
    type t = {api_compatible : int; api_level : int; api_prerelease : bool; major : int; minor: int; patch : int; } [@@deriving protocol ~driver:(module Msgpack), sexp]
  end

  module Types = struct
    module Type_ = struct
      type t = {
        id : int;
        prefix : string;
      }[@@deriving protocol ~driver:(module Msgpack), sexp]
    end

    type t = Type_.t String_map_msgpack.t [@@deriving protocol ~driver:(module Msgpack), sexp]

    let to_mli _ = "\n\nval ext_types : (int * string) list\n"
    let to_ml t = 
      "let ext_types = [\n"
      ^ String.concat ~sep:"" (List.map (Map.to_alist t) ~f:(fun (name, {Type_.id; prefix = _}) -> sprintf "%d, \"%s\";\n" id name))
      ^ "]\n;;"
      ^ "let () = Object.set_ext_types ext_types"

  end


  type t = { error_types : Error_type.t String_map_msgpack.t
           ; functions : Func.t list
           ; version : Version.t 
           ; types : Types.t
           } [@@deriving protocol ~driver:(module Msgpack), sexp]


  let group_functions_by_module t =
    let (g, m) =
      List.partition_map t.functions ~f:(fun func -> match Func.which_module func with | None -> `Fst func | Some m -> `Snd (m, func))
    in
    g, 
    String.Map.of_alist_multi m
  ;;

  let func_list_to_mli funcs = 
    String.concat ~sep:"\n"
      (List.map funcs ~f:(fun f -> Func.to_mli f))


  let to_mli t = 
    let (global_funcs, methods) = 
      group_functions_by_module t
    in
    let method_mli = 
      Map.to_alist methods
      |> List.map ~f:(fun (md, funcs) -> 
          sprintf "module %s : sig\ninclude (module type of %s)\n%s\nend\n\n" md md (indent (func_list_to_mli funcs)))
      |> String.concat ~sep:""
    in
    method_mli ^ func_list_to_mli global_funcs ^ (Types.to_mli t.types)
  ;;

  let func_list_to_ml funcs = 
    String.concat ~sep:"\n;;\n\n"
      (List.map funcs ~f:(fun f -> Func.to_ml f))
  ;;

  let to_ml t = 
    let (global_funcs, methods) = 
      group_functions_by_module t
    in
    let method_ml = 
      Map.to_alist methods
      |> List.map ~f:(fun (md, funcs) -> 
          sprintf "module %s = struct\ninclude %s\n%s\nend\n\n" md md (indent (func_list_to_ml funcs)))
      |> String.concat ~sep:""
    in
    method_ml ^ func_list_to_ml global_funcs ^ (Types.to_ml t.types)
  ;;

  let prepare t =
    { t with functions = 
    List.sort t.functions ~compare:(fun x y -> String.compare (Func.ocaml_name x) (Func.ocaml_name y))
    }
  ;;
end

let conn_sig = 
  {|
    Conn : sig 
      type t 
      type 'a ret
      val call : t -> name:string -> Msgpack.t -> (Msgpack.t -> 'a) -> 'a ret
    end
  |}

let preamble = {|
open! Protocol_conv_msgpack
open! Import

|}


let main ~ml_target ~mli_target ~source =
  Reader.file_contents source
  >>= fun cont -> 
  let (_, t) = Msgpck.String.read cont in
  printf !"%s\n\n________________\n" (Msgpck.show t);
  Writer.flushed (force Writer.stdout)
  >>= fun () ->
  let protocol = Protocol.of_msgpack t |> Protocol.prepare in
  Deferred.all_unit
    [
      Writer.save ml_target ~contents:(preamble ^ Protocol.to_ml protocol);
      Writer.save mli_target ~contents:(preamble ^ Protocol.to_mli protocol);
      Writer.save (mli_target ^ ".txt") ~contents:(Msgpck.show t);
    ]
  (*printf "%s" (Msgpck.String.to_string (Rpc.to_msgpack (Rpc.create ~name:"nvim_command" (to_msgpack ["echo 'test'"]))));*)

let () =
  let open Command.Let_syntax in
  Command.run
    (Command.async ~summary:"Generate nvim syntax"
       ([%map_open
          let ml_target = anon ("mlfile" %: string) 
          and mli_target = anon ("mlifile" %: string) 
          and source = flag "-nvim-api-info" (required file) ~doc:"FILE output of nvim --api-info"
          in
          fun () -> 
          main ~ml_target ~mli_target ~source
          >>= fun () ->
          Writer.flushed (force Writer.stdout)
          >>| fun () ->
          Shutdown.shutdown 0]))
        ;;

(*
   :so vimrc
*)
