open Core
open Async

type t = {
  waiting : unit Rpc_message.t Ivar.t Int.Table.t 
  ; writer: Writer.t
  ; handler : (t -> unit Rpc_message.t -> unit)
  }


module Handler = struct
  type nonrec t = t -> unit Rpc_message.t -> unit
end

let next_message_id = ref 100

let start_loop ?(verbose=false) t r = 
  Reader.read_one_chunk_at_a_time r ~handle_chunk:(fun bigstr ~pos ~len -> 
      let s = Bigstring.to_string ~pos ~len bigstr in
      let (n, msgs) = Msgpck.String.read_all s in
      List.iter msgs ~f:(fun msg -> 
          if verbose then Log.Global.info "Msgpack from nvim: %s\n\n" (Msgpck.show msg);
          let rpc = Rpc_message.of_msgpack_unit msg  in
          match Hashtbl.find t.waiting rpc.id with
          | None -> t.handler t rpc
          | Some ivar -> 
            Ivar.fill ivar rpc;
            Hashtbl.remove t.waiting rpc.id);
      if n = 0 
      then return `Continue
      else return (`Consumed (n, `Need_unknown)))
  >>| ignore
;;


let embed ?verbose ?working_dir ?(nvim_exe_path="nvim") ~handler () = 
  Process.create ?working_dir ~prog:nvim_exe_path ~args:["--embed"; "--headless"] ()
  >>| function 
  | Error e -> (Error e)
  | Ok p -> 
    let waiting = Int.Table.create () in
    let t = 
      { writer = Process.stdin p; waiting; handler }
    in
    don't_wait_for (start_loop ?verbose t (Process.stdout p));
    (Ok t)
;;

let connect_tcp ?verbose ~handler host_and_port =
  let%bind (_, r,writer) = Tcp.connect host_and_port in
  let waiting = Int.Table.create () in
  let t = 
    { writer; waiting; handler }
  in
  don't_wait_for (start_loop ?verbose t r);
  return t
;;

let call t name payload ret =
  Monitor.try_with (fun () -> 
      let id = !next_message_id in
      incr next_message_id;
      let ivar = Ivar.create () in
      Hashtbl.set t.waiting ~key:id ~data:ivar;
      Writer.write t.writer (Msgpck.String.to_string (Rpc_message.to_msgpack {Rpc_message.id; request = 0; name; payload }));
      Ivar.read ivar
      >>| fun (data:unit Rpc_message.t) -> 
      Ok (ret data.payload))
  >>| function
  | Ok (Ok x) -> Ok x
  | Ok (Error x) -> Error x
  | Error exn -> Error (Error.tag ~tag:("While processing call " ^ name) (Error.of_exn exn))
;;
  
type 'a result = 'a Or_error.t Deferred.t
