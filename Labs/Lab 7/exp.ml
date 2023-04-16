type 'a infstream = Cons of 'a * (unit -> 'a infstream)

(* Returns the first n elements of an infinite stream as a list *)
let rec take n (Cons (h, t)) =
  if n <= 0 then []
  else h :: take (n - 1) (t ())

(* Modified unfold function *)
let rec unfold f x inc fact =
  let (v, x') = f x inc fact in
  Cons (v, fun () -> unfold f x' (inc+1) (fact * (inc+1)))

(* 1a - Makes an infinite stream of terms in the infinite series of x *)
let exp_terms x = unfold (fun (a, b) inc fact -> (a, (b, ((x ** float_of_int(inc)) /. (float_of_int(fact)))))) (1.,x) 2 2

(* 1b - Calculates the sum of the exponential of 1.1 up to 20 terms *)
let sum = List.fold_left ( +. ) 0. (1.1 |> exp_terms |> take 20)


