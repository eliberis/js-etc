open Async.Std;;

let () =
    let command = Command.async
        ~summary: "etc trading program"
        Command.Spec.(
            empty
            +> flag "-host" (optional_with_default "localhost" string)
                ~doc:"Hostname"
            +> flag "-port" (optional_with_default 25000 int)
                ~doc:"Port"
            +> flag "-name" (optional_with_default "TEAM_NAME" string)
                ~doc:"Team name"
        )
        (fun host port team_name () ->
           printf "Hello from %s at %s:%d\n" team_name host port;
           return ()
        )
    in
    Command.run command;;
