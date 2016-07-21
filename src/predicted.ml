open Core.Std
open Async.Std
open Message

let limit = function
    | Symbol.VALE | VALBZ -> 10
    | _ -> 100
;;

let fair controller ~symbol =
    let aux symbol = 
        let cmp x y = Price.compare (x.Controller.Trade.price) (y.Controller.Trade.price) in
        let trades = Controller.last_trades controller ~symbol ~limit:8000 in
        let trades = List.sort ~cmp trades in
        match trades with
        | trades when List.length trades > 5 ->
            let len = List.length trades in
            let trades = List.(take (drop trades (len / 4)) (3 * len / 4)) in
            let sum, cnt = List.fold trades ~init:(0, 0) ~f:(fun (sum, cnt) trade ->
                (sum + trade.Controller.Trade.price * trade.size, cnt + trade.size))
            in
            Some (sum / cnt)
        | _ -> None
    in
    match symbol with
    | Symbol.BOND -> Some 1000
    (*| VALE -> aux Symbol.VALBZ*)
    | symbol -> aux symbol
;;

let penny ~symbol ?(margin=1) controller = function
    | Message.Server.Fill _ | Message.Server.Open | Message.Server.Hello _ ->
        let aux ~dir ~price =
            let pos = Controller.position controller ~dir ~symbol |> Int.abs in
            if pos < limit symbol then
                Controller.add controller
                    ~symbol
                    ~dir
                    ~price
                    ~size:(limit symbol - pos)
                |> Deferred.ignore
            else
                return ()
        in
        begin
        match fair controller ~symbol, Controller.trading_range controller ~symbol with
        | (Some fair, Some (min, max)) when min <= fair - margin && fair + margin <= max ->
            printf "%d %d\n" min max;
            aux ~dir:Direction.Buy ~price:(fair - margin)
            >>= fun _ ->
            aux ~dir:Direction.Sell ~price:(fair + margin)
            |> Deferred.ignore
        | _ -> return ()
        end
    | _ -> return ()
;;
