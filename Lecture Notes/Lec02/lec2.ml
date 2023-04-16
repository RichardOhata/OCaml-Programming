(* length of list *)
let rec length l =
  match l with
  | [] -> 0
  | _::xs -> 1 + length xs

(* tail-recursive version of length *)
let length2 l =
  let rec length' acc l =
    match l with
    | [] -> acc
    | _::xs -> length' (acc + 1) xs
  in
  length' 0 l

(* reversing a list *)
let rec reverse l =
  match l with
  | [] -> []
  | x::xs -> reverse xs @ [x]

(* tail-recursive version of reverse *)
let reverse2 l =
  let rec reverse' acc l =
    match l with
    | [] -> acc
    | x::xs -> reverse' (x::acc) xs
  in
  reverse' [] l      

(* return the first n elements of a list *)
let rec take n l =
  if n <= 0 then []
  else (
    match l with
    | [] -> []
    | x::xs -> x::take (n - 1) xs
  )

let take2 n l =
  let rec take' acc n l =
    match l with
    | [] -> reverse acc
    | _ when n <= 0 -> reverse acc  (* guard *)
    | x::xs -> take' (x::acc) (n - 1) xs
  in
  take' [] n l   

(* test whether 2 lists are equal *)  
let rec equal l l' =
  match l, l' with
  | [], [] -> true
  | [], _ | _, [] -> false
  | x::xs, x'::xs' when x <> x' -> false
  | _::xs, _::xs' -> equal xs xs'

(* head of list *)  
let head =  (* partial function - not defined when list is empty *)
  function
  | [] -> failwith "head: empty list"
  | x::_ -> x     

let head_opt =  (* total function - defined for all lists *)
  function
  | [] -> None
  | x::_ -> Some x

(* look for a specific key in a list of key-values *)  
let rec find_opt k =
  function
  | [] -> None
  | (k', v)::kvs when k = k' -> Some v
  | _::kvs -> find_opt k kvs       

(* - inserting an element into a sorted list preserving its order
   - list is in ascending order *)  
let rec insert x l =
  match l with
  | [] -> [x]
  | y::ys when x > y -> y::insert x ys
  | _ -> x::l  

let rec insertion_sort =
  function
  | [] -> []
  | x::xs -> insert x @@ insertion_sort xs  
    
