open Async.Std

module Bond_strategy = struct
  let run controller =
    (* TODO send initial set of orders *)
    ()


  let react_fill controller order =
    (* TODO: depending on whether it was buy or sell, put more buys or sells *)
    ()
end
