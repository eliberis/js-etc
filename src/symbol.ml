open Async.Std

module Symbol = struct
  type t = BOND | VALE | VALBZ with sexp
end
