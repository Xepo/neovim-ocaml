open! Core
open! Async
module M2 = Tcp

let%expect_test "get_buffer is working" = 
  begin 
    let open Deferred.Or_error.Let_syntax in
    let%bind conn = Nvim.Conn.embed ~handler:(fun _ -> failwith "Handler not implemented") ~config:`None ()
    in
    let%bind () = Nvim.Nvim_protocol.command ~conn ~command:"e /tmp/test" in
    let%bind buf = 
      Nvim.Nvim_protocol.get_current_buf ~conn
    in
    printf "%d\n" buf;
    let%bind () = Nvim.Nvim_protocol.command ~conn ~command:"e /tmp/test2" in
    let%map new_buf = 
      Nvim.Nvim_protocol.get_current_buf ~conn
    in
    printf "%d\n" new_buf;
  end
  >>| Or_error.ok_exn
  >>= fun () -> 
  [%expect {|
    1
    2 |}]
;;

let%expect_test "get_buffer is working" = 
  begin 
    let open Deferred.Or_error.Let_syntax in
    let%bind conn = Nvim.Conn.embed ~handler:(fun _ -> failwith "Handler not implemented") ~config:`None ()
    in
    let%bind () = Nvim.Nvim_protocol.command ~conn ~command:"e /tmp/test" in
    let%bind buf = 
      Nvim.Nvim_protocol.get_current_buf ~conn
    in
    printf "%d\n" buf;
    let%bind () = Nvim.Nvim_protocol.command ~conn ~command:"e /tmp/test2" in
    let%map new_buf = 
      Nvim.Nvim_protocol.get_current_buf ~conn
    in
    printf "%d\n" new_buf;
  end
  >>| Or_error.ok_exn
  >>= fun () -> 
  [%expect {|
    1
    2 |}]
;;
(*
   :so vimrc
*)
