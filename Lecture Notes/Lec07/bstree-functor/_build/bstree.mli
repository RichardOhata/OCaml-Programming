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

module Make(Ord : OrderedType) : S with type elt = Ord.t
