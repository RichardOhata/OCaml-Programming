let rec alternate1 l =
  match l with
  | [] -> []
  | [x] -> [x]
  | x1::x2::xs -> x1 :: alternate1 xs

let rec alternate2 =
  function
    | x1::x2::xs -> x1 :: alternate2 xs
    | l -> l

let rec alternate3 l =
  match l with
  | [] | [_] -> l
  | x1::x2::xs -> x1 :: alternate3 xs

let rec fib n =
  if n = 0 then 0
  else if n = 1 then 1
  else
    fib (n - 1) + fib (n - 2)

let fib2 n =
  let rec fib' i a b =
    if i = n then a
    else
      fib' (i + 1) b (a + b)
  in
  fib' 0 0 1

(* count_changes 100 [25; 10; 5] *)
let rec count_changes amt denoms =
  if amt < 0 then 0
  else if amt = 0 then 1
  else (
    match denoms with
    | [] -> 0
    | d::ds ->
        count_changes (amt - d) denoms +  (* use d *)
        count_changes amt ds              (* don't use d *)
  )

(* some higher order functions
   -- most of these are already in the List module
*)
let rec map f l =
  match l with
  | [] -> []
  | x::xs -> f x :: map f xs


let rec filter f l =
  match l with
  | [] -> []
  | x::xs when f x -> x :: filter f xs
  | _::xs -> filter f xs

(* drop_while is not in the List module *)  
let rec drop_while f l =
  match l with
  | [] -> []
  | x::xs when f x -> drop_while f xs
  | _ -> l

(* think of this as processing a list from left to right *)  
let rec fold_left f a l =
  match l with
  | [] -> a
  | x::xs -> fold_left f (f a x) xs

(* think of this as processing a list from right to left *)  
let rec fold_right f l a =
  match l with
  | [] -> a
  | x::xs -> f x (fold_right f xs a)
