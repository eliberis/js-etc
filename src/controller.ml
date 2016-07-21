open Async.Std
open Bot_state

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

  let handle_message _controller _message =
    (* match on message type and call appropriate handle functions *)
    ()

  let send_order _controller ~symbol:_ ~dir:_ ~price:_ ~size:_ =
    (* TODO actually send order *)
    return ()
end
