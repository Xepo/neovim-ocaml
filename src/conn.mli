open! Core
open! Async

type t

module Handler : sig
  type nonrec t = t -> unit Rpc_message.t -> unit
end

(** Spins up installed nvim embedded and headless **)
val embed : ?verbose:bool -> ?working_dir:string -> ?nvim_exe_path:string -> handler:Handler.t -> unit -> t Or_error.t Deferred.t 
val connect_tcp : ?verbose:bool -> handler:Handler.t -> _ Tcp.Where_to_connect.t -> t Deferred.t
(* val connect_stdio : unit -> t *)

type 'a result = 'a Or_error.t Deferred.t

val call : t -> string -> Msgpck.t -> (Msgpck.t -> 'a) -> 'a result
