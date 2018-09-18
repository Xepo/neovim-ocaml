open! Core
open! Async
module M2 = Tcp

let%expect_test "" = 
  Nvim.Conn.embed ~handler:(fun _ -> failwith "Handler not implemented") ()
  >>| Or_error.ok_exn
  >>= fun conn -> 
  Nvim.Nvim_protocol.get_current_buf ~conn
  >>| Or_error.ok_exn
  >>= fun x -> 
  printf "%d" x;
  [%expect {| 1 |}]
(*
   :so vimrc
*)
