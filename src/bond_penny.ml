open Core.Std
open Async.Std
open Controller
open Message

let run controller = function
    | Message.Server.Hello _ ->
        Controller.add controller
            ~symbol:Symbol.BOND
            ~dir:Direction.Buy
            ~price:999
            ~size:10
        >>= fun _ ->
        Controller.add controller
            ~symbol:Symbol.BOND
            ~dir:Direction.Sell
            ~price:1001
            ~size:10
        |> Deferred.ignore
    | Message.Server.Fill _ ->
        let aux ~dir ~price =
            let pos = Controller.position controller ~dir ~symbol:Symbol.BOND in
            if pos < 10 then
                Controller.add controller
                    ~symbol:Symbol.BOND
                    ~dir
                    ~price
                    ~size:(10 - pos)
                |> Deferred.ignore
            else
                return ()
        in
        aux ~dir:Direction.Buy ~price:999
        >>= fun _ ->
        aux ~dir:Direction.Sell ~price:1001
        |> Deferred.ignore
    | _ -> return ()
