open! Core.Std
open! Async.Std
open Message

module Book = Server.Book;;

type t
type callback = t -> Message.Server.t -> unit Deferred.t

val run
   : reader:Reader.t
  -> writer:Writer.t
  -> team_name:string
  -> callbacks:(callback list)
  -> 'a Deferred.t

val add
   : t
  -> symbol:Symbol.t
  -> dir:Direction.t
  -> price:Price.t
  -> size:Size.t
  -> Order_id.t Deferred.t

val convert
   : t
  -> symbol:Symbol.t
  -> dir:Direction.t
  -> size:Size.t
  -> Order_id.t Deferred.t

val cancel
   : t
  -> Order_id.t
  -> unit Deferred.t

val book
   : t
  -> Symbol.t
  -> Book.t Option.t

val position
   : t
  -> ?dir:Direction.t
  -> symbol:Symbol.t
  -> int
