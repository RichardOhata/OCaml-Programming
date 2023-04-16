let return x = [x]

let ( >>= ) l f = List.concat_map f l

let guard cond l =
  if cond then l else []

let multiply_to n =
  List.init n (( + ) 1) >>= fun x ->
    List.init n (( + ) 1) >>= fun y ->
      guard (x + y = n) [(x, y)]
