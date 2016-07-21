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

    let to_string = function
        | Buy -> "BUY"
        | Sell -> "SELL"
end

module Symbol = struct
    module Inner = struct
        type t = BOND | VALBZ | VALE | GS | MS | WFC | XLF with sexp
        let compare = Pervasives.compare
    end

    include Inner
    include Sexpable.To_stringable(Inner)
    include Comparable.Make(Inner)
end

module Server = struct
    module Hello = struct
        type t =
            { cash : int
            ; symbols : int Symbol.Map.t
            } with sexp
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
        | Hello of Hello.t
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
            { Hello.
              cash = json |> member "cash" |> to_int
            ; symbols = json |> member "symbols" |> to_list |> List.fold ~init:Symbol.Map.empty ~f:(fun acc json ->
                Symbol.Map.add acc
                  ~key:(json |> member "symbol" |> to_string |> Symbol.of_string)
                  ~data:(json |> member "position" |> to_int))
            }
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
end

module Client = struct
    module Hello = struct
        type t = string with sexp
    end;;

    module Add = struct
        type t =
            { order_id : Order_id.t
            ; symbol : Symbol.t
            ; dir : Direction.t
            ; price : Price.t
            ; size : Size.t
            } with sexp
    end;;

    module Convert = struct
        type t =
            { order_id : Order_id.t
            ; symbol : Symbol.t
            ; dir : Direction.t
            ; size : Size.t
            } with sexp
    end;;

    module Cancel = struct
        type t = Order_id.t with sexp
    end;;

    type t =
        | Hello of Hello.t
        | Add of Add.t
        | Convert of Convert.t
        | Cancel of Cancel.t
        with sexp

    let to_string =
        let open Yojson.Basic in
        function
        | Hello name -> to_string
            (`Assoc
                [ "type", `String "hello"
                ; "team", `String name
                ])
        | Add add -> to_string
            (`Assoc
                [ "type", `String "add"
                ; "order_id", `Int add.order_id
                ; "symbol", `String (Symbol.to_string add.symbol)
                ; "dir", `String (Direction.to_string add.dir)
                ; "price", `Int add.price
                ; "size", `Int add.size
                ])
        | Convert convert -> to_string
            (`Assoc
                [ "type", `String "convert"
                ; "order_id", `Int convert.order_id
                ; "symbol", `String (Symbol.to_string convert.symbol)
                ; "dir", `String (Direction.to_string convert.dir)
                ; "size", `Int convert.size
                ])
        | Cancel cancel -> to_string
            (`Assoc
                [ "type", `String "cancel"
                ; "order_id", `Int cancel
                ])
end;;
