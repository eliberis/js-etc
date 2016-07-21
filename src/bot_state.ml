open Async.Std
open Positions
open Order

module Bot_state = struct
  type t = {
    positions : Positions.t;
    orders : Order.t list;
  }
end
