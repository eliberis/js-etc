open Async.Std
open Bot_state

module Controller = struct
  type t = { mutable state : Bot_state.t }

  let handle_hello controller message =
    (* Pattern match on Hello and update positions *)
    ()

  let handle_open controller message =
    ()

  let handle_close controller message =
    ()

  let handle_message controller message =
    (* match on message type and call appropriate handle functions *)
    ()
end
