type 'a lazystream = Cons of 'a * 'a lazystream lazy_t

let rec from n = Cons (n, lazy (from (n + 1)))

let nats = from 0

let hd (Cons (h, _)) = h

let tl (Cons (_, t)) = Lazy.force t

let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h :: take (n - 1) (Lazy.force t)

let rec drop n ((Cons (h, t)) as s) =
  if n <= 0 then s
  else drop (n - 1) (Lazy.force t)

let rec map f (Cons (h, t)) =
  Cons (f h, lazy (map f (Lazy.force t)))

(*
let rec map2 f (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (f h1 h2, lazy (map2 f (Lazy.force t1) (Lazy.force t2)))
*)

let rec map2 f s1 s2 =
  Cons (f (hd s1) (hd s2), lazy (map2 f (tl s1) (tl s2)))

let rec fibs = Cons (1, lazy (Cons (1, lazy (map2 ( + ) fibs (tl fibs)))))

let rec unfold f x =
  let (v, x') = f x in
  Cons (v, lazy (unfold f x'))

