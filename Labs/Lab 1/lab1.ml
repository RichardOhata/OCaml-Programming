(* 1a *)
(* Returns list with n elements dropped. *)
let rec drop n l = 
  if n <= 0 then l
  else (
    match l with
    | [] -> []
    | h::t -> drop (n-1) t
  )

(* 1b *)
(* Helper function to reverse a list *)
let reverse2 l =
  let rec reverse' acc l =
    match l with
    | [] -> acc
    | x::xs -> reverse' (x::acc) xs
  in
  reverse' [] l   

(* Returns a list consisting of pairs of corresponding elements from list 1 and list 2. *)
let zip lst1 lst2 =
  let rec zip' acc lst1 lst2 = 
    match lst1, lst2 with
    | [], [] -> reverse2 acc
    | [], _ | _, [] -> reverse2 acc 
    | h::t, h'::t' -> zip' ((h, h')::acc) t t' 
  in
  zip' [] lst1 lst2

(* 1c *)
(* Takes a list of pairs and returns a pair of lists where the pairs in the inital lists have been divded into. *)
let unzip lst = 
  let rec unzip' acc acc2 lst =
    match lst with
    | [] ->  (reverse2 acc, reverse2 acc2)
    | (h, h')::t -> unzip' (h::acc) (h'::acc2) t
  in
  unzip' [] [] lst  

(* 1d *)
(* Removes consecutive duplicated elements in a list, collpased into one element. *)
let rec dedup lst =
  match lst with 
  | a::(b::_ as t) -> if a = b then dedup t else a::dedup t
  | _ -> lst

(* 2. *)   
(* Helper function to calculate the factorial *)
let factorial_tr n =
  let rec factorial n acc =
    if n <= 0 then acc
    else factorial (n - 1) (n * acc)
  in
  factorial n 1

(* Evalutes the exponenetial using n and x. *)
let exp n x =
    let rec exp' acc n x =
      if n > 0 then exp' (acc +. ((x ** float_of_int(n-1)) /. (float_of_int(factorial_tr (n-1))))) (n-1) x
      else acc
      in 
      exp' 0. n x

  