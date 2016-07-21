open Async.Std
open Message
open Symbol

module Bond_strategy = struct
  let run ~send_order =
    (* TODO send initial set of orders *)
    send_order
      ~symbol:Symbol.BOND
      ~dir:Direction.Buy
      ~price:999
      ~size:100
    >>= fun () ->
    send_order
      ~symbol:Symbol.BOND
      ~dir:Direction.Sell
      ~price:1001
      ~size:100

  let react_fill ~send_order fill =
    match fill.Server.Fill.dir with
    | Direction.Sell ->
      send_order
        ~symbol:Symbol.BOND
        ~dir:Direction.Buy
        ~price:999
        ~size:fill.size
    | Direction.Buy ->
      send_order
        ~symbol:Symbol.BOND
        ~dir:Direction.Sell
        ~price:1001
        ~size:fill.size

end
