(* binary search trees *)
type 'a bstree = Leaf | Node of 'a * 'a bstree * 'a bstree

let rec bst_size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + bst_size l + bst_size r

let rec bst_height t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (bst_height l) (bst_height r)

let rec bst_insert x t =
  match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (x', l, r) when x < x' -> Node (x', bst_insert x l, r)
  | Node (x', l, r) when x > x' -> Node (x', l, bst_insert x r)
  | _ -> t

let bst_of_list l =
  List.fold_left (fun a x -> bst_insert x a) Leaf l

let rec bst_mem x t =
  match t with
  | Leaf -> false
  | Node (x', l, _) when x < x' -> bst_mem x l
  | Node (x', _, r) when x > x' -> bst_mem x r
  | _ -> true

let rec bst_largest t =
  match t with
  | Leaf -> failwith "bst_largest: empty tree"
  | Node (x, _, Leaf) -> x
  | Node (_, _, r) -> bst_largest r

let rec bst_delete x t =
  match t with
  | Leaf -> Leaf
  | Node (x', l, r) when x < x' -> Node (x', bst_delete x l, r)
  | Node (x', l, r) when x > x' -> Node (x', l, bst_delete x r)
  | Node (_, Leaf, r) -> r
  | Node (_, l, Leaf) -> l
  | Node (_, l, r) ->
      let succ = bst_largest l in
      Node (succ, bst_delete succ l, r)


