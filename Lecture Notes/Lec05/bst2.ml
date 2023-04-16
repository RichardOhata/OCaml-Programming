(* binary search trees with comparison function *)
type 'a bstree = Leaf | Node of 'a * 'a bstree * 'a bstree

let rec bst_size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + bst_size l + bst_size r

let rec bst_height t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (bst_height l) (bst_height r)

let rec bst_insert ~cmp x t =
  match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (x', l, r) when cmp x x' < 0 -> Node (x', bst_insert ~cmp x l, r)
  | Node (x', l, r) when cmp x x' > 0 -> Node (x', l, bst_insert ~cmp x r)
  | _ -> t

let bst_of_list ~cmp l =
  List.fold_left (fun a x -> bst_insert ~cmp x a) Leaf l

let rec bst_mem ~cmp x t =
  match t with
  | Leaf -> false
  | Node (x', l, _) when cmp x x' < 0 -> bst_mem ~cmp x l
  | Node (x', _, r) when cmp x x' > 0 -> bst_mem ~cmp x r
  | _ -> true

let rec bst_maximal t =
  match t with
  | Leaf -> failwith "bst_maximal: empty tree"
  | Node (x, _, Leaf) -> x
  | Node (_, _, r) -> bst_maximal r

let rec bst_delete ~cmp x t =
  match t with
  | Leaf -> Leaf
  | Node (x', l, r) when cmp x x' < 0 -> Node (x', bst_delete ~cmp x l, r)
  | Node (x', l, r) when cmp x x' > 0 -> Node (x', l, bst_delete ~cmp x r)
  | Node (_, Leaf, r) -> r
  | Node (_, l, Leaf) -> l
  | Node (_, l, r) ->
      let succ = bst_maximal l in
      Node (succ, bst_delete ~cmp succ l, r)


