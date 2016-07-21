open Async.Std
open Bot_state
open Bond_strategy
open Message

module Controller = struct
  type t = { mutable state : Bot_state.t;
             writer : Network.writer;
             mutable counter : int
           }

  let on_connect =
    (* Write hello *)
    ()

  let handle_hello _controller _message =
    (* Pattern match on Hello and update positions *)
    ()

  let handle_open _controller _message =
    ()

  let handle_close _controller _message =
    ()

  let send_order controller ~symbol ~dir ~price ~size =
    let order = { Client.Add.
                  order_id = controller.counter;
                  symbol; dir; price; size }
    in
    controller.counter <- controller.counter + 1;
    controller.writer (Client.to_string (Client.Add order))

  let handle_fill controller fill =
    let send_order = send_order controller in
    Bond_strategy.react_fill ~send_order fill

  let handle_message controller message =
    match message with
    | Server.Fill fill -> handle_fill controller fill
    | _ -> return ()

end
