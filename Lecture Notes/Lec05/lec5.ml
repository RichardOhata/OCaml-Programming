type 'a mylist = Nil | Cons of 'a * 'a mylist  (* recursive *)

(* let l =  Cons (3, Cons (2, (Cons (7, Cons (6, Cons (8, Nil)))))) *)

let rec iter f l =
  match l with
  | Nil -> ()
  | Cons (x, xs) -> f x; iter f xs

let rec map f l =
  match l with
  | Nil -> Nil
  | Cons (x, xs) -> Cons (f x, map f xs)

let rec fold_left f a l =
  match l with
  | Nil -> a
  | Cons (x, xs) -> fold_left f (f a x) xs

(* binary search tree of integers *)  
type tree = Leaf | Node of int * tree * tree

let rec size t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size l + size r

let rec height t =
  match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (height l) (height r)

let rec insert x t =
  match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (x', l, r) when x < x' -> Node (x', insert x l, r)
  | Node (x', l, r) when x > x' -> Node (x', l, insert x r)
  | _ -> t

let of_list l =
  List.fold_left (fun a x -> insert x a) Leaf l

let rec mem x t =
  match t with
  | Leaf -> false
  | Node (x', l, _) when x < x' -> mem x l
  | Node (x', _, r) when x > x' -> mem x r
  | _ -> true

