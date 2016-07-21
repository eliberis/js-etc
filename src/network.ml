open Core.Std;;
open Async.Std;;
open Message;;

type writer = string -> unit Deferred.t;;

let loop ~host ~port ~f ~on_connect =
    let addr = Tcp.to_host_and_port host port in
    let rec loop () =
        try_with (fun () -> Tcp.with_connection addr (fun _socket reader writer ->
            let write_and_flush line = Writer.write_line writer line; Writer.flushed writer in
            on_connect write_and_flush
            >>= fun () ->
            Pipe.iter (Reader.lines reader) ~f:(fun line -> f ~line ~write:write_and_flush)
            >>= fun () ->
            raise End_of_file))
        >>= function
            | Ok _ -> failwith "Finished?"
            | Error err ->
                    print_endline (Exn.to_string err);
                    Clock.after (Time.Span.of_sec 1.)
                    >>= fun () ->
                    printf "Reconnecting...\n%!";
                    loop ()
    in
    loop ()
;;
