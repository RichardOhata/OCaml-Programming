(* Key-value tree *)
type ('k, 'v) kvtree = Leaf | Node of ('k * 'v) * ('k, 'v) kvtree * ('k, 'v) kvtree


(* Inserts a node into the tree *)
let rec kvt_insert t ~cmp k v =
  match t with
  | Leaf -> Node ((k, v), Leaf, Leaf)
  | Node ((k', v'), l, r) when cmp k k' = 0 -> Node ((k', v), l, r)
  | Node ((k', v'), l, r) when cmp k k' < 0 -> Node ((k', v'), kvt_insert l ~cmp k v, r)
  | Node ((k', v'), l, r) when cmp k k' > 0 -> Node ((k', v'), l, kvt_insert r ~cmp k v)
  | _ -> t
  
(* Searches for and returns the value of a specific key *)
let rec kvt_find (t : ('a, 'b) kvtree) ~cmp (k : 'a) =
  match t with
  | Node ((k', v), l, _) when cmp k k' < 0 -> kvt_find l ~cmp k
  | Node ((k', v), _, r) when cmp k k' > 0 -> kvt_find r ~cmp k
  | Node ((k', v), _, _) when cmp k k' = 0 -> Some v
  | _ -> None

(* Helper function to find the largest node *)
let rec kvt_maximal t =
  match t with
  | Leaf -> failwith "kvt_maximal: empty tree"
  | Node ((k, v), _, Leaf) -> (k,v)
  | Node (_, _, r) -> kvt_maximal r

(* Deletes a specific key node from the tree *)
let rec kvt_delete t ~cmp k =
  match t with
  | Leaf -> Leaf
  | Node ((k', v), l, r) when cmp k k' < 0 -> Node ((k', v), kvt_delete l ~cmp k, r); 
  | Node ((k', v), l, r) when cmp k k' > 0 -> Node ((k', v), l, kvt_delete r ~cmp k)
  | Node (_, Leaf, r) -> r
  | Node (_, l, Leaf) -> l
  | Node (_, l, r) ->
      let (succK, succV) = kvt_maximal l in
      Node ((succK, succV), kvt_delete l ~cmp succK, r)

(* Creates a key-value tree from a list *)
 let kvt_of_list lst ~cmp =
  List.fold_left (fun t (k, v) -> kvt_insert t ~cmp k v) Leaf lst 