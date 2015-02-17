type 'a graph = ('a * 'a list) list;;

let insert_vtx u g = (u, []) :: g;;

let rec insert_edge u v = function
  []                     -> []
| (h, l) :: t when h = u -> (u, v :: l) :: t
| x :: t                 -> x :: insert_edge u v t;;

let rec has_edges_to u = function
  []                     -> []
| (h, l) :: t when h = u -> l
| _ :: t                 -> has_edges_to u t;;

let rec has_edges_from u = function
  []                            -> []
| (h, l) :: t when List.mem u l -> h :: has_edges_from u t
| _ :: t                        -> has_edges_from u t;;
