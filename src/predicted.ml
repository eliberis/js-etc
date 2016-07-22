open Core.Std
open Async.Std
open Message

let limit = function
    | _ -> 10
;;

let initial_price sym =
  match (sym : Symbol.t) with
  | AMZN -> 24000
  | HD -> 11800
  | DIS -> 9400
  | PG -> 7500
  | KO -> 4100
  | PM -> 8500
  | NEE -> 10500
  | DUK -> 7200
  | SO -> 4700
  | _ -> assert false

let fair controller ~symbol =
    let aux symbol =
        let cmp x y = Price.compare (x.Controller.Trade.price) (y.Controller.Trade.price) in
        let trades = Controller.last_trades controller ~symbol ~limit:100 in
        let trades = List.sort ~cmp trades in
        match trades with
        | trades when List.length trades > 5 ->
            let len = List.length trades in
            let trades = List.(take (drop trades (len / 4)) (3 * len / 4)) in
            let sum, cnt = List.fold trades ~init:(0, 0) ~f:(fun (sum, cnt) trade ->
                (sum + trade.Controller.Trade.price * trade.size, cnt + trade.size))
            in
            sum / cnt
        | _ -> initial_price symbol
    in
    match (Symbol.basket symbol) with
    | Some (etf, consts) ->
      List.fold consts ~init:0 ~f:(fun price_so_far (sym, share) ->
          price_so_far + share * (aux sym) ) / etf
    | None -> aux symbol
;;

let penny ~symbol ?(margin=1) controller = function
    | Message.Server.Fill _ | Message.Server.Open | Message.Server.Hello _ | Message.Server.Book _ ->
        let aux ~dir ~price =
            let pos = Controller.position controller ~dir ~symbol |> Int.abs in
            if pos < limit symbol then
                Controller.add controller
                    ~symbol
                    ~dir
                    ~price
                    ~size:(limit symbol - pos)
                >>= fun order_id ->
                after (sec 2.)
                >>= fun () ->
                Controller.cancel controller order_id
            else
                return ()
        in
        begin
        match fair controller ~symbol, Controller.trading_range controller ~symbol with
        | (fair, Some (min, max)) when min <= fair - margin && fair + margin <= max ->
            printf "%d %d\n" min max;
            aux ~dir:Direction.Buy ~price:(fair - margin)
            >>= fun _ ->
            aux ~dir:Direction.Sell ~price:(fair + margin)
            |> Deferred.ignore
        | _ -> return ()
        end
    | _ -> return ()
;;
