(* type 'a t = Leaf | Node of 'a * 'a t * 'a t *)
type 'a t
val size : 'a t -> int
val height : 'a t -> int
val insert : 'a -> 'a t -> 'a t
val of_list : 'a list -> 'a t
val mem : 'a -> 'a t -> bool
val delete : 'a -> 'a t -> 'a t
