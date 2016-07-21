open Core.Std
open Async.Std
open Symbol

module Positions = struct
  type t = {
    stocks : (Symbol.t, int) List.Assoc.t;
    cash : int
  }
end
