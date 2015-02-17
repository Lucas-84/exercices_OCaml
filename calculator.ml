(* Calculator *)
type key = Addition | Subtraction | Multiplication | Division | Equal | Digit of int;;
type state = {last_key : key; last_operator : key; last_result : int; result : int};;

let evaluate x y = function
    Addition        -> x + y
  | Subtraction     -> x - y
  | Multiplication  -> x * y
  | Division        -> x / y
  | Equal           -> y
  | _               -> failwith "evaluate: unknown operator"

let transition s k = match (k, s.last_key) with
    (Digit x, Digit y) -> {s with last_key = Digit y; result = y * 10 + x}
  | (Digit x, k)       -> {s with last_key = k; result = x}
  | (k, _)             -> let ans = evaluate s.last_result s.result s.last_operator
                          in {last_key = k; last_operator = k; last_result = ans; result = ans};;

let transition_list l =
  let initial_state = {last_key = Equal; last_operator = Equal; last_result = 0; result = 0}
  in List.fold_left transition initial_state l;;
  
transition_list [Digit 5; Multiplication; Digit 3; Addition; Digit 18; Equal];;
