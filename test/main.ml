open Core
open Async
module M2 = Tcp

 (*module X : (module type of Msgpck.String) = struct type t end*)
let main ~where_to_connect =
  let%bind conn = 
    Nvim.Conn.connect_tcp ~handler:(fun _ _ -> failwith "not implemented handler") where_to_connect
  in
  Nvim.Nvim_protocol.nvim_command ~conn ~command:"echo 'Hi!'"
  >>| Or_error.ok_exn
  >>= fun () -> 
  let%bind buffer = 
    Nvim.Nvim_protocol.vim_get_current_buffer ~conn 
    >>| Or_error.ok_exn
  in
  Nvim.Nvim_protocol.Buffer.nvim_buf_set_lines ~conn buffer ~strict_indexing:true ~start:0 ~end_:0 ~replacement:["Whoa"]
  >>| Or_error.ok_exn
      >>= fun () -> 
  Nvim.Nvim_protocol.nvim_set_current_line ~conn ~line:"neato"
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
