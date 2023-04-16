let return x = Some x

let ( >>= ) m f =
  match m with
  | None -> None
  | Some x -> f x

let lift2 f mx my =
  mx >>= fun x ->
    my >>= fun y ->
      return @@ f x y

let ( + ) = lift2 Stdlib.( + )
let ( - ) = lift2 Stdlib.( - )
let ( * ) = lift2 Stdlib.( * )

let div mx my =
  mx >>= fun x ->
    my >>= fun y ->
      if y = 0 then None else Some (x / y)

let ( / ) = div

(* e.g. Some 2 * Some 3 + Some 1 *)
