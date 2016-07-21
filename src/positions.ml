open Async.Std
open Symbol

module Positions = struct
  type t = {
    stocks : Symbol.t list;
    cash : int
  }
end
