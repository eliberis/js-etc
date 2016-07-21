open Async.Std
open Message
open Symbol
open Controller

module Bond_strategy = struct
  let run controller =
    (* TODO send initial set of orders *)
    Controller.send_order controller
      ~symbol:Symbol.BOND
      ~dir:Direction.Buy
      ~price:999
      ~size:100
    >>= fun () ->
    Controller.send_order controller
      ~symbol:Symbol.BOND
      ~dir:Direction.Sell
      ~price:1001
      ~size:100

  let react_fill controller order =
    match order.dir with
    | Direction.Sell ->
      Controller.send_order controller
        ~symbol:Symbol.BOND
        ~dir:Direction.Buy
        ~price:999
        ~size:order.size
    | Direction.Buy ->
      Controller.send_order controller
        ~symbol:Symbol.BOND
        ~dir:Direction.Sell
        ~price:1001
        ~size:order.size

end
