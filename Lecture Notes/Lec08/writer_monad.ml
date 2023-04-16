type 'a writer = 'a * string

let return x = (x, "")

let ( >>= ) (a, s) f =
  let (b, s') = f a in
  (b, s ^ s')

let ( >> ) m1 m2 = m1 >>= fun _ -> m2

let tell s = (0, s)

let inc x = (x + 1, Printf.sprintf "inc %d = %d; " x (x + 1))

let dec x = (x - 1, Printf.sprintf "dec %d = %d; " x (x - 1))
