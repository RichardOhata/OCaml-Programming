module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type Kvtree = sig
  type elt
  type 'a t = Leaf | Node of (elt * 'a) * 'a t * 'a t

  val insert : elt -> 'a -> 'a t -> 'a t
  val find : elt -> 'a t -> 'a option
  val delete : elt -> 'a t -> 'a t
  val of_list : (elt * 'a) list -> 'a t
  val empty : 'a t
  val is_empty : 'a t -> bool
  val to_list : 'a t -> (elt * 'a) list
end

module Make(Ord : OrderedType) = struct
  type elt = Ord.t
  type 'a t = Leaf | Node of (elt * 'a) * 'a t * 'a t

  (* Inserts a key value pair into the tree *)
  let rec insert k v t = 
    match t with
    | Leaf -> Node ((k, v), Leaf, Leaf)
    | Node ((k', v'), l, r) when Ord.compare k k' = 0 -> Node ((k', v), l, r)
    | Node ((k', v'), l, r) when Ord.compare k k' < 0 -> Node ((k', v'), insert k v l, r)
    | Node ((k', v'), l, r) when Ord.compare k k' > 0 -> Node ((k', v'), l, insert k v r)
    | _ -> t

  (* Finds a key value pair from the tree *)
  let rec find k t =
    match t with
    | Node ((k', v), l, _) when Ord.compare k k' < 0 -> find k l
    | Node ((k', v), _, r) when Ord.compare k k' > 0 -> find k r
    | Node ((k', v), _, _) when Ord.compare k k' = 0 -> Some v
    | _ -> None
  
  (* Helper function to determine the greatest key value pair *)
  let rec maximal t =
    match t with
    | Leaf -> failwith "maximal: empty tree"
    | Node ((k, v), _, Leaf) -> (k,v)
    | Node (_, _, r) -> maximal r  
  
  (* Deletes a key value pair from the tree *)
  let rec delete k t =
    match t with
    | Leaf -> Leaf
    | Node ((k', v), l, r) when Ord.compare k k' < 0 -> Node ((k', v), delete k l , r)
    | Node ((k', v), l, r) when Ord.compare k k' > 0 -> Node ((k', v), l, delete k r)
    | Node (_, Leaf, r) -> r
    | Node (_, l, Leaf) -> l
    | Node (_, l, r) ->
        let (succK, succV) = maximal l in
        Node ((succK, succV), delete succK l, r)    
  
  (* Creates a tree from a list *)
  let of_list lst =
    List.fold_left (fun t (k, v) -> insert k v t) Leaf lst 

  (* Returns an empty tree *)
  let empty =
    Leaf
  
  (* Checks whether a tree is empty or not *)
  let is_empty t = 
    match t with
    | Leaf -> true
    | _ -> false
  
  (* Traverses through the tree in preorder *)
  let rec preorder t =
    match t with
    | Leaf -> []
    | Node ((k, v), l, r) -> [(k, v)] @ preorder l @ preorder r
  
  (* Returns a list representing the tree in ascending order of keys *)
  let to_list t = 
   List.sort (fun (k, _) (k', _) -> Ord.compare k k') (preorder t)

end