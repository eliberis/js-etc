open Async.Std
open Bot_state
open Bond_strategy

module Controller = struct
  type t = { mutable state : Bot_state.t }

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

  let handle_fill controller fill =
    let send_order = Controller.send_order controller in
    Bond_strategy.react_fill ~send_order fill

  let handle_message controller message =
    match message with
    | Server.Fill fill -> handle_fill controller fill
    | _ -> return ()

  let send_order _controller ~symbol:_ ~dir:_ ~price:_ ~size:_ =
    (* TODO actually send order *)
    return ()
end
