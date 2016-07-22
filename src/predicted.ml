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
  let trade_history_price symbol =
    printf "Trade history price requested";
    let cmp x y = Price.compare (x.Controller.Trade.price) (y.Controller.Trade.price) in
    let trades = Controller.last_trades controller ~symbol ~limit:8 in
    let trades = List.sort ~cmp trades in
    match trades with
    | trades when List.length trades >= 4 ->
      let len = List.length trades in
      let trades = List.(take (drop trades (len / 4)) (2 * len / 4)) in
      let sum, cnt = List.fold trades ~init:(0, 0) ~f:(fun (sum, cnt) trade ->
          (sum + trade.Controller.Trade.price * trade.size, cnt + trade.size))
      in
      Some (sum / cnt)
    | _ -> None
  in
  let book_price symbol =
    match Controller.trading_range controller ~symbol with
    | None -> None
    | Some (best_bid, best_offer) -> Some ((best_bid + best_offer) / 2)
  in
  let initial_const_price symbol = initial_price symbol
  in
  let aux symbol =
    match book_price symbol with
    | Some p -> p
    | None -> match trade_history_price symbol with
      | Some p -> p
      | None -> initial_const_price symbol
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
          let pos = Controller.position controller ~dir ~symbol in
          let plimit = limit symbol in
          let size = match dir with | Buy -> plimit - pos | Sell -> pos + plimit in
          if size > 0 then
            Controller.add controller
                ~symbol
                ~dir
                ~price
                ~size:(min size 2)
            >>= fun order_id ->
            upon (Clock.after (sec 2.0)) (fun () -> Controller.cancel controller order_id |> don't_wait_for);
            return ()
          else begin
            printf !"Did not send order because of position %d limit %d" pos (limit symbol);
            return ()
          end
        in
        begin
        let position = Controller.position controller ~symbol ?dir:None in
        let spread_threshold = 10 in
        match fair controller ~symbol, Controller.trading_range controller ~symbol with
        | (fair, Some (min, max)) when min <= fair && fair <= max && max - min > 0 ->
            printf "fair: %d, %d %d: %d\n" fair min max margin;
            (if position < 0 || max - min > spread_threshold then aux ~dir:Direction.Buy ~price:(min + 1) else return ())
            >>= fun _ ->
            (if position > 0 || max - min > spread_threshold then aux ~dir:Direction.Sell ~price:(max - 1) else return ())
            |> Deferred.ignore
        | (fair, Some (min, max)) ->
            printf "fair outside range: %d not in [%d, %d]\n" fair min max;
            return ()
        | _ ->
            printf "Some other weird case\n";
            return ()
        end
    | _ -> return ()
;;
