open Core.Std;;
open Async.Std;;
open Message;;

module Book = Server.Book;;
module Trade = Server.Trade;;

module Order = struct
  type t = Add of Client.Add.t | Convert of Client.Convert.t
end;;

type t =
  { mutable books : Book.t Symbol.Map.t
  ; mutable orders : Order.t Order_id.Map.t
  ; mutable order_id : Order_id.t
  ; mutable trades : (Trade.t list) Symbol.Map.t
  ; mutable cash : int
  ; mutable positions : int Symbol.Map.t
  ; writer : Message.Client.t -> unit Deferred.t
  }

type callback = t -> Message.Server.t -> unit Deferred.t

let conversion_rate = 100
;;

let _basket sym =
  match (sym : Symbol.t) with
  | x -> (1, [(x, 1)]) (* TODO: add proper baskets *)

let internal_callback controller = function
  | Message.Server.Book book ->
      controller.books <- Symbol.Map.add controller.books ~key:book.symbol ~data:book;
      return ()
  | Message.Server.Reject reject ->
      controller.orders <- Order_id.Map.remove controller.orders reject.order_id;
      printf "Order %d has been rejected: %s\n" (reject.order_id) (reject.error);
      return ()
  | Message.Server.Fill fill ->
      begin
      match Order_id.Map.find controller.orders fill.order_id with
      | None -> return ()
      | Some (Add add) ->
          assert (fill.dir = add.dir);
          assert (fill.symbol = add.symbol);
          assert (fill.size <= add.size);
          controller.orders <- Order_id.Map.add controller.orders
              ~key:fill.order_id ~data:(Add { add with size = add.size - fill.size });
          let signed = (match fill.dir with | Buy -> fill.size | Sell -> -fill.size) in

          controller.cash <- controller.cash - signed * fill.price;
          controller.positions <- Symbol.Map.change controller.positions fill.symbol
            ~f:(fun x -> Option.value_map ~default:signed ~f:(( + ) signed) x |> Option.some);
          return ()
      | Some (Convert convert) ->
          assert (fill.dir = convert.dir);
          assert (fill.symbol = convert.symbol);
          assert (fill.size <= convert.size);
          controller.orders <- Order_id.Map.add controller.orders
              ~key:fill.order_id ~data:(Convert { convert with size = convert.size - fill.size });
          (* TODO *)
          controller.cash <- controller.cash - conversion_rate;
          (*controller.positions <- Symbol.Map.change controller.positions fill.symbol
              ~f:(fun x -> )*)
          return ()
      end
  | Message.Server.Out order_id ->
      controller.orders <- Order_id.Map.remove controller.orders order_id;
      return ()
  | Message.Server.Trade trade ->
      controller.trades <- Symbol.Map.change controller.trades trade.symbol
        ~f:(Option.value_map ~default:(Some [trade]) ~f:(fun trades -> Some (trade :: trades)));
      return ()
  | _ -> return ()

let run ~reader ~writer ~team_name ~callbacks =
  let controller =
    { books = Symbol.Map.empty
    ; orders = Order_id.Map.empty
    ; order_id = 1
    ; trades = Symbol.Map.empty
    ; cash = 0
    ; positions = Symbol.Map.empty
    ; writer = (fun msg ->
      printf "sending %s\n" (Message.Client.to_string msg);
      Message.Client.to_string msg |> Writer.write_line writer;
      Writer.flushed writer)
    }
  in
  controller.writer (Message.Client.Hello team_name)
  >>= fun () ->
  Pipe.iter (Reader.lines reader) ~f:(fun line ->
    let message = Message.Server.of_string line in
    internal_callback controller message
    >>= fun () ->
    List.map callbacks ~f:(fun f -> f controller message)
    |> Deferred.all_unit)
  >>= fun () ->
  raise End_of_file
;;

let next_order_id controller =
  let order_id = controller.order_id in
  controller.order_id <- controller.order_id + 1;
  order_id
;;

let add controller ~symbol ~dir ~price ~size =
  assert (size > 0);
  assert (price > 0);
  let order_id = next_order_id controller in
  let order =
    { Message.Client.Add.
      order_id
    ; symbol
    ; dir
    ; price
    ; size
    }
  in
  controller.orders <- Order_id.Map.add controller.orders ~key:order_id ~data:(Order.Add order);
  controller.writer (Message.Client.Add order)
  >>| fun () ->
  order_id
;;

let convert controller ~symbol ~dir ~size =
  let order_id = next_order_id controller in
  let order =
    { Message.Client.Convert.
      order_id
    ; symbol
    ; dir
    ; size
    }
  in
  controller.orders <- Order_id.Map.add controller.orders ~key:order_id ~data:(Order.Convert order);
  controller.writer (Message.Client.Convert order)
  >>| fun () ->
  order_id
;;

let cancel controller order_id =
  controller.writer (Message.Client.Cancel order_id)
;;

let book controller order_id =
  Symbol.Map.find controller.books order_id
;;

let position controller ?dir ~symbol =
  let init =
    Symbol.Map.find controller.positions symbol
    |> Option.value ~default:0
  in
  match dir with
  | None -> init
  | Some dir ->
    Order_id.Map.fold controller.orders ~init ~f:(fun ~key:_ ~data acc ->
      match data with
      | Add add when add.dir = dir && add.symbol = symbol ->
          acc + (match add.dir with | Buy -> 1 | Sell -> -1) * add.size
      | Convert _ -> acc (* TODO *)
      | _ -> acc)
;;

let last_trades controller ~symbol ~limit =
  Symbol.Map.find controller.trades symbol
  |> Option.value_map ~default:[] ~f:(fun l -> List.take l limit)
;;

let trading_range controller ~symbol =
  let open Option in
  Symbol.Map.find controller.books symbol
  >>= fun book ->
  let buy_max = List.fold book.buy ~init:Int.min_value ~f:(fun acc entry -> Int.max acc entry.price)
  and sell_min = List.fold book.sell ~init:Int.max_value ~f:(fun acc entry -> Int.min acc entry.price)
  in
  return (buy_max, sell_min)
;;
