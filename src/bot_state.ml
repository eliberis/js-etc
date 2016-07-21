open Async.Std
open Positions
open Order
open Symbol

module Bot_state = struct
  type t = {
    positions : Positions.t;
    orders : Order.t list;
  }

  let estimate_fair_value symbol =
    match symbol with
    | Symbol.BOND -> 1000
    | _ -> 0 (* TODO *)
end
