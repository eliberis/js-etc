open Core.Std
open Async.Std
open Controller
open Message

let limit = 100;;

let run controller = function
    | Message.Server.Fill _ | Message.Server.Open | Message.Server.Hello _ ->
        let aux ~dir ~price =
            let pos = Controller.position controller ~dir ~symbol:Symbol.BOND |> Int.abs in
            if pos < limit then
                Controller.add controller
                    ~symbol:Symbol.BOND
                    ~dir
                    ~price
                    ~size:(limit - pos)
                |> Deferred.ignore
            else
                return ()
        in
        aux ~dir:Direction.Buy ~price:999
        >>= fun _ ->
        aux ~dir:Direction.Sell ~price:1001
        |> Deferred.ignore
    | _ -> return ()
;;
