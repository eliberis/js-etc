open! Core.Std;;
open! Async.Std;;

type writer = string -> unit Deferred.t;;

val loop
    : host:string
    -> port:int
    -> f:(line:string -> write:writer -> unit Deferred.t)
    -> on_connect:(writer -> unit Deferred.t)
    -> 'a Deferred.t
;;


