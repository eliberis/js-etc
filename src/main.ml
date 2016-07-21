open Core.Std
open Async.Std
open Controller

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
           printf "Hello from %s at %s:%d\n" team_name host port;
           Network.loop ~host ~port
               ~f:(fun ~line ~write:_ ->
                   print_endline line;
                   print_endline (Message.Server.of_string line |> Message.Server.sexp_of_t |> Sexp.to_string);
                   return ()
               )
               ~on_connect:(fun write ->
                   Controller.on_connect;
                   Message.Client.Hello team_name |> Message.Client.to_string |> write)
        )
    in
    Command.run command;;
