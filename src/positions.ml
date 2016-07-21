open Core.Std
open Async.Std
open Message

module Positions = struct
  type t = {
    stocks : (Symbol.t, int) List.Assoc.t;
    cash : int
  }
end
