(* 
   notes:
   - all functions take 1 argument; however, a function can return another
    function
   - -> is right associative, e.g. int -> int -> int is the same as
     int -> (int -> int)
   - function application is left associative e.g. add 1 2 is the same as
     (add 1) 2
   - function application binds tighter than operators, e.g.
     f 1 -2 is the same as (f 1) - 2 NOT f (1 - 2)
   - tail-recursive vs non-tail recursive functions, e.g.
     * non-tail recursive
     fact 3 = 3 * fact 2 
            = 3 * (2 * fact 1) 
             = 3 * (2 * (1 * fact 0))
            = 3 * (2 * (1 * 1))
            = 3 * (2 * 1)
            = 3 * 2
            = 6
      * tail-recursive
      fact_tr 3 1 = fact_tr 2 3
                  = fact_tr 1 6
                  = fact_tr 0 6
                  = 6
*)                  

let rec factorial n =
  if n <= 0 then 1
  else n * factorial (n - 1)

let factorial_tr n =
  let rec factorial n acc =
    if n <= 0 then acc
    else factorial (n - 1) (n * acc)
  in
  factorial n 1
  
let mysqrt x =
  let next y = 0.5 *. (y +. x /. y) in
  let close_enough y = abs_float (y *. y -. x) < 0.000001 in
  let rec iter y =
    if close_enough y then y
    else iter (next y)
  in
  iter 1.

let subtract x y = x - y


(* twice f is a function that applies f twice to its argument *)  
let twice f x = f (f x)

(* flip the arguments to a function *)
let flip f x y = f y x

(* 2 useful operators *)
let ($) f x = f x
let (|>) x f = f x  (* already defined by the system *)


