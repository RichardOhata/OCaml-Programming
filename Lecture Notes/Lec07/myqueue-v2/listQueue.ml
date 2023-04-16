module M : Myqueue.S = struct
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

