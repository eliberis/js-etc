open Core.Std;;

module Order_id = struct
    type t = int with sexp
end

module Price = struct
    type t = int with sexp
end

module Size = struct
    type t = int with sexp
end

module Direction = struct
    type t = Buy | Sell with sexp

    let of_string = function
        | "BUY" -> Buy
        | "SELL" -> Sell
        | _ -> failwith "wrong direction"
end

module Symbol = struct
    module Inner = struct
        type t = BOND | VALBZ | VALE | GS | MS | WFC | XLF with sexp
    end

    include Inner
    include Sexpable.To_stringable(Inner)
end

module Reject = struct
    type t =
        { order_id : Order_id.t
        ; error: string
        } with sexp
end

module Book = struct
    module Entry = struct
        type t =
            { price : Price.t
            ; size : Size.t
            } with sexp
    end

    type t =
        { symbol : Symbol.t
        ; buy : Entry.t list
        ; sell : Entry.t list
        } with sexp
end;;

module Trade = struct
    type t =
        { symbol : Symbol.t
        ; price : Price.t
        ; size : Size.t
        } with sexp
end;;

module Fill = struct
    type t =
        { order_id : Order_id.t
        ; symbol : Symbol.t
        ; dir : Direction.t
        ; size : Size.t
        } with sexp
end;;

module Ack = struct
    type t = Order_id.t with sexp
end;;

module Out = struct
    type t = Order_id.t with sexp
end;;
    

type t =
    | Hello
    | Error of string
    | Reject of Reject.t
    | Book of Book.t
    | Trade of Trade.t
    | Open (* hmm? *)
    | Close (* same here *)
    | Ack of Ack.t
    | Fill of Fill.t
    | Out of Out.t
    with sexp

let of_string str =
    let open Yojson.Basic in
    let open Yojson.Basic.Util in
    let json = from_string str in
    match json |> member "type" |> to_string with
    | "hello" -> Hello
    | "error" -> Error (json |> member "error" |> to_string)
    | "reject" -> Reject
        { Reject.
          order_id = json |> member "order_id" |> to_int
        ; error = json |> member "error" |> to_string
        } 
    | "trade" -> Trade
        { Trade.
          price = json |> member "price" |> to_int
        ; size = json |> member "size" |> to_int
        ; symbol = json |> member "symbol" |> to_string |> Symbol.of_string
        }
    | "ack" -> Ack (json |> member "order_id" |> to_int)
    | "open" -> Open (* meh *)
    | "close" -> Close
    | "book" ->
        let process arr = match arr |> to_list with
            | [price; size] ->
                { Book.Entry.
                  price = to_int price
                ; size = to_int size
                }
            | _ -> failwith "wtf in book entry"
        in Book
        { Book.
          symbol = json |> member "symbol" |> to_string |> Symbol.of_string
        ; sell = json |> member "sell" |> to_list |> List.map ~f:process
        ; buy = json |> member "buy" |> to_list |> List.map ~f:process
        }
    | "fill" -> Fill
        { Fill.
          order_id = json |> member "order_id" |> to_int
        ; symbol = json |> member "symbol" |> to_string |> Symbol.of_string
        ; dir = json |> member "dir" |> to_string |> Direction.of_string
        ; size = json |> member "size" |> to_int
        }
    | "out" -> Out (json |> member "order_id" |> to_int)
    | msg -> failwith ("illegal message type: " ^ msg)
