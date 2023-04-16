(* implementing binary search trees using records (& variants) *)
type 
'a bstree = Leaf | Node of 'a node
and
'a node = {value: 'a; left: 'a bstree; right: 'a bstree}

let rec bst_insert ~cmp x t =
  match t with
  | Leaf -> Node {value = x; left = Leaf; right = Leaf}
  | Node {value; left; right} when cmp x value < 0 ->
      Node {value; left = bst_insert ~cmp x left; right}
  | Node {value; left; right} when cmp x value > 0 ->
      Node {value; left; right = bst_insert ~cmp x right}
  | _ -> t

let bst_of_list ~cmp l =
  List.fold_left (fun t x -> bst_insert ~cmp x t) Leaf l
