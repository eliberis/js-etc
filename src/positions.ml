open Core.Std
open Async.Std
open Message

module Positions = struct
  type t =
    { stocks : int Symbol.Map.t
    ; cash : int
    }
end
