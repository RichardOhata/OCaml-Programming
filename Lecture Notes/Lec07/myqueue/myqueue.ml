module type MyQueue = sig
  type 'a t

  exception Empty
  val empty : 'a t
  val is_empty : 'a t -> bool
  val enqueue : 'a -> 'a t -> 'a t
  val dequeue : 'a t -> 'a t
  val front : 'a t -> 'a
  val front_opt : 'a t -> 'a option
end

module ListQueue : MyQueue = struct
  type 'a t = 'a list

  exception Empty

  let empty = []

  let is_empty q = q = []

  let enqueue x q = q @ [x]

  let dequeue q =
    match q with
    | [] -> raise Empty
    | _::xs -> xs

  let front q =
    match q with
    | [] -> raise Empty
    | x::_ -> x

  let front_opt q =
    match q with
    | [] -> None
    | x::_ -> Some x
end

module TwoListQueue : MyQueue = struct
  type 'a t = 'a list * 'a list

  exception Empty

  let empty = ([], [])

  let is_empty (l, _) = l = []

  let enqueue x (l1, l2) =
    if l1 = [] then ([x], [])
    else (l1, x::l2)

  let dequeue q =
    match q with
    | [], _ -> raise Empty
    | [x], l -> (List.rev l, [])
    | x::xs, l -> (xs, l)

  let front q =
    match q with
    | [], _ -> raise Empty
    | x::_, _ -> x

  let front_opt q =
    match q with
    | [], _ -> None
    | x::_, _ -> Some x
end

