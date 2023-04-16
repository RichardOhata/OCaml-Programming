type 'a lazystream = Cons of 'a * 'a lazystream lazy_t

(* Creates an infinite stream that starts at n *)
let rec from n = Cons (n, lazy (from (n + 1)))

(* Returns the head of an infinite stream *)
let hd (Cons (h, _)) = h

(* Returns the tail of an infinite stream *)
let tl (Cons (_, t)) = Lazy.force t

(* Returns the first n elements of an infinite stream as a list *)
let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h :: take (n - 1) (Lazy.force t)

(* 2a - Applies a boolean function on a stream and filters out the elements that don't fulfill the functions condition *)
let rec filter f (Cons (h, t)) =
  if (f h) then Cons (h, lazy (filter f (Lazy.force t)))
  else Cons (hd (Lazy.force t), lazy (filter f (tl (Lazy.force t))))

(* 2b - Creates an infinite sequence of prime numbers *)
let primes = 
let rec primes' (Cons (h, t) as s) inc = 
  let p = filter (fun x -> if (x mod inc <> 0 || x == inc) then true else false) s in
  Cons (hd p, lazy (primes' (tl p) (inc+1)))
in primes'
(from 2) 2
  
  


  