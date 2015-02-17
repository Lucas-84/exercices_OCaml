(* Merging two lists *)

let rec merge_i la lb = match (la, lb) with
    ([], _)              -> lb
  | (_, [])              -> la
  | (ha :: ta, hb :: tb) -> if ha < hb then ha :: merge_i ta lb
                            else hb :: merge_i la tb;;

let rec merge f la lb = match (la, lb) with
    ([], _)              -> lb
  | (_, [])              -> la
  | (ha :: ta, hb :: tb) -> if f ha hb then ha :: merge f ta lb
                            else hb :: merge f la tb;;
                            
type 'a mylist = {l: 'a list; f: 'a -> 'a -> bool; s: bool};;

let myinsert ml e =
  let rec aux = function
      [] -> [e]
    | h :: t -> if ml.f e h then e :: h :: t else h :: aux t
  in {ml with l = aux ml.l};;

let mysort ml = 
  if ml.s then ml
  else List.fold_left myinsert {ml with l = []; s = true} ml.l;;

let mymerge mla mlb = {l = merge mla.f (mysort mla).l (mysort mlb).l; s = true; f = mla.f};;
