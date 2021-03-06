open Core
open Async
module M2 = Tcp

 (*module X : (module type of Msgpck.String) = struct type t end*)
let main ~where_to_connect =
  let%bind conn = 
    Nvim.Conn.connect_tcp ~handler:(fun _ _ -> failwith "not implemented handler") where_to_connect
  in
  Nvim.command ~conn ~command:"echo 'Hi!'"
  >>| Or_error.ok_exn
  >>= fun () -> 
  let%bind buffer = 
    Nvim.get_current_buf ~conn 
    >>| Or_error.ok_exn
  in
  Nvim.Buffer.set_lines ~conn buffer ~strict_indexing:true ~start:0 ~end_:0 ~replacement:["Whoa"]
  >>| Or_error.ok_exn
      >>= fun () -> 
  Nvim.set_current_line ~conn ~line:"neato"
  >>| Or_error.ok_exn
;;

let () =
  let open Command.Let_syntax in
  Command.run
    (Command.async ~summary:"Generate nvim syntax"
       ([%map_open
         let where_to_connect = anon ("where_to_connect" %: string) 
         in
         fun () -> 
           main ~where_to_connect:(Tcp.Where_to_connect.of_host_and_port (Host_and_port.t_of_sexp (Sexp.of_string where_to_connect)))
           >>= fun () ->
           Writer.flushed (force Writer.stdout)
           >>| fun () ->
           Shutdown.shutdown 0]))
;;

(*
   :so vimrc
*)
