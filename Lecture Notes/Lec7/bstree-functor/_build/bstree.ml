module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  (* type t *)
  type t = Leaf | Node of elt * t * t

  val size : t -> int
  val insert : elt -> t -> t
  val delete : elt -> t -> t
  val mem : elt -> t -> bool
  val of_list: elt list -> t
end

module Make(Ord : OrderedType) = struct
  type elt = Ord.t
  type t = Leaf | Node of elt * t * t

  let rec size t =
    match t with
  | Leaf -> 0
  | Node (_, l, r) -> 1 + size l + size r

  let rec insert x t =
    match t with
  | Leaf -> Node (x, Leaf, Leaf)
  | Node (x', l, r) when Ord.compare x x' < 0 -> 
      Node (x', insert x l, r)
  | Node (x', l, r) when Ord.compare x x' > 0 -> 
      Node (x', l, insert x r)
  | _ -> t

  let of_list l =
    List.fold_left (fun a x -> insert x a) Leaf l

  let rec mem x t =
    match t with
  | Leaf -> false
  | Node (x', l, _) when Ord.compare x x' < 0 -> mem x l
  | Node (x', _, r) when Ord.compare x x' > 0 -> mem x r
  | _ -> true

  let rec maximal t =
    match t with
  | Leaf -> failwith "maximal: empty tree"
  | Node (x, _, Leaf) -> x
  | Node (_, _, r) -> maximal r

  let rec delete x t =
    match t with
  | Leaf -> Leaf
  | Node (x', l, r) when Ord.compare x x' < 0 -> 
      Node (x', delete x l, r)
  | Node (x', l, r) when Ord.compare x x' > 0 ->
      Node (x', l, delete x r)
  | Node (_, Leaf, r) -> r
  | Node (_, l, Leaf) -> l
  | Node (_, l, r) ->
      let succ = maximal l in
      Node (succ, delete succ l, r)
end

