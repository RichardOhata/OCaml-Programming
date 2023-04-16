(* binary search trees; motivation for maybe monad *)
type 'a bstree = Leaf | Node of 'a * 'a bstree * 'a bstree

let rec bst_insert x t =
  match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (x', l, r) when x < x' -> Node (x', bst_insert x l, r)
  | Node (x', l, r) when x > x' -> Node (x', l, bst_insert x r)
  | _ -> t

let bst_of_list l =
  List.fold_left (fun a x -> bst_insert x a) Leaf l

let right_right t =
  match t with
  | Leaf -> None
  | Node (_, _, r) ->
      match r with
      | Leaf -> None
      | Node (_, _, r') -> Some r'

let left t =
  match t with
  | Leaf -> None
  | Node (_, l, _) -> Some l

let right t =
  match t with
  | Leaf -> None
  | Node (_, _, r) -> Some r

(* combining left and right *)
let left_right t =
  match left t with
  | None -> None
  | Some l -> right l

(* val return : 'a -> 'a option *)  
let return x = Some x

(* val bind : 'a option -> ('a -> 'b option) -> 'b option *)
let bind m f = 
  match m with
  | None -> None
  | Some x -> f x

let ( >>= ) = bind  
