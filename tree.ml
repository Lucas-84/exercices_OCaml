(* Lexical tree *)

type lex_node = Letter of char * bool * lex_tree and lex_tree = lex_node list;;

let rec exists d w = let n = String.length w in match d with
  [] -> false
| Letter (h, e, s) :: _ when h = w.[0] -> if n = 1 then e
                                          else exists s (String.sub w 1 (n - 1))
| _ :: t -> exists t w;;

let rec insert d w =
  let n = String.length w in
  if n = 0 then d
  else let ws = String.sub w 1 (n - 1) in match d with
    [] -> [Letter (w.[0], n = 1, insert [] ws)]
  | Letter (h, e, s) :: t when h = w.[0] -> Letter (h, n = 1 || e, insert s ws) :: t
  | x :: t -> x :: insert t w;;
  
let construct l = List.fold_left insert [] l;;
let verify l d = List.fold_left (fun s w -> if exists d w then s else w :: s) [] l;;
let string_of_char c = String.make 1 c;;

let rec select d n = match d with
  [] -> []
| Letter (h, e, s) :: t -> let sh = string_of_char h in
                           if n = 1 && e then sh :: select t n
                           else if n = 1 then select t n
                           else select t n @ List.map ((^) sh) (select s (n - 1));;
