open! Core
open! Async

type t

module Handler : sig
  type nonrec t = t -> unit Rpc_message.t -> unit
end


val connect_tcp : handler:Handler.t -> _ Tcp.Where_to_connect.t -> t Deferred.t
(* val connect_stdio : unit -> t *)

type 'a result = 'a Or_error.t Deferred.t

val call : t -> string -> Msgpck.t -> (Msgpck.t -> 'a) -> 'a result
