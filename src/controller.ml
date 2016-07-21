open Core.Std
open Async.Std
open Bot_state
open Bond_strategy
open Message
open Positions

module Controller = struct
  type t = { mutable state : Bot_state.t;
             mutable writer : Message.Client.t -> unit Deferred.t;
             mutable counter : int
           }

  let on_connect controller ~team_name writer =
    controller.writer <- Fn.compose writer Client.to_string;
    controller.writer (Client.Hello team_name) 
  ;;

  let init () =
    let state = 
      { Bot_state.
        positions =
          { Positions.
            stocks = Symbol.Map.empty
          ; cash = 0
          }
      }
    in
    { state
    ; writer = (fun _ -> failwith "not initialised")
    ; counter = 0
    }
  ;;

  let handle_hello controller hello =
    controller.state <- { Bot_state.
      positions = let open Server.Hello in
        { Positions.
          stocks = hello.symbols
        ; cash = hello.cash
        }
    };
    Bond_strategy.run ~send_order
  ;;

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
    controller.writer (Client.Add order)

  let handle_fill controller fill =
    let send_order = send_order controller in
    Bond_strategy.react_fill ~send_order fill

  let handle_message controller message =
    match message with
    | Server.Fill fill -> handle_fill controller fill
    | Server.Hello hello -> handle_hello controller hello
    | _ -> return ()

end
