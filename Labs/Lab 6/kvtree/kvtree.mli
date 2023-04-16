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

module Make(Ord : OrderedType) : Kvtree with type elt = Ord.t