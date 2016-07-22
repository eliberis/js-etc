open Core.Std
open Async.Std
open Controller
open Message

let () =
    let command = Command.async
        ~summary: "etc trading program"
        Command.Spec.(
            empty
            +> flag "-host" (optional_with_default "localhost" string)
                ~doc:"Hostname"
            +> flag "-port" (optional_with_default 25000 int)
                ~doc:"Port"
            +> flag "-name" (optional_with_default "COULOMB" string)
                ~doc:"Team name"
        )
        (fun host port team_name () ->
            let addr = Tcp.to_host_and_port host port in
            Tcp.with_connection addr (fun _socket reader writer ->
                Controller.run ~reader ~writer ~team_name
                    ~callbacks:
                        [ (fun _controller msg ->
                            match msg with
                            | Fill _ -> print_endline (Message.Server.sexp_of_t msg |> Sexp.to_string) |> return
                            | _ -> return ())
                        ; Predicted.penny ~symbol:Symbol.AMZN ~margin:1
                        ; Predicted.penny ~symbol:Symbol.DIS ~margin:1
                        ; Predicted.penny ~symbol:Symbol.DUK ~margin:1
                        ; Predicted.penny ~symbol:Symbol.HD ~margin:1
                        ; Predicted.penny ~symbol:Symbol.KO ~margin:1
                        ; Predicted.penny ~symbol:Symbol.NEE ~margin:1
                        ; Predicted.penny ~symbol:Symbol.PG ~margin:1
                        ; Predicted.penny ~symbol:Symbol.PM ~margin:1
                        (* ; Predicted.penny ~symbol:Symbol.RSP ~margin:1 *)
                        ; Predicted.penny ~symbol:Symbol.SO ~margin:1
                        (* ; Predicted.penny ~symbol:Symbol.XLP ~margin:3 *)
                        (*; Predicted.penny ~symbol:Symbol.XLU ~margin:1
                        ; Predicted.penny ~symbol:Symbol.XLY ~margin:1*)
                        ])
        )
    in
    Command.run command;;
