(* Binary Search Tree *)
type 'a bstree = Leaf | Node of 'a * 'a bstree * 'a bstree

(* Inserts a node into the tree *)
let rec bst_insert x t =
  match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (x', l, r) when x < x' -> Node (x', bst_insert x l, r)
  | Node (x', l, r) when x > x' -> Node (x', l, bst_insert x r)
  | _ -> t

(* Creates a tree from a list *)
let bst_of_list l =
  List.fold_left (fun a x -> bst_insert x a) Leaf l

  
(* All 3 functions print out the order of values from each visit to a node *)
(* Traverses through the tree in preorder *)
let rec bst_preorder (f : 'a -> unit) tree = 
  match tree with 
  | Leaf -> ()  
  | Node (x, l, r) -> f x; bst_preorder f l; bst_preorder f r

(* Traverses through the tree in inorder *)
let rec bst_inorder (f : 'a -> unit) tree = 
  match tree with 
  | Leaf -> ()  
  | Node (x, l, r) -> bst_inorder f l; f x; bst_inorder f r

(* Traverses through the tree in postorder *)
let rec bst_postorder (f : 'a -> unit) tree = 
  match tree with 
  | Leaf -> ()  
  | Node (x, l, r) -> bst_postorder f l; bst_postorder f r; f x