(* 1a *)
(* Returns the smallest element in a non-empty list *)
let min_elt lst =
  let rec min_elt' min lst =
    match lst with
    | [] -> min
    | h::t when (h < min) -> min_elt' h t
    | h::t -> min_elt' min t 
  in 
  min_elt' (List.hd(lst)) lst

(* 1b *)
(* Removes the first occurance of 'x' in a list. *)
let remove x l =
  let rec remove' acc x l =
  match l with
  | [] -> acc
  | h::t when (h = x) -> acc @ t
  | h::t -> remove' (acc @ [h]) x t
  in
  remove' [] x l

(* 1c *)
(* Performs selection sort on a list, orders the list from smallest to largest *)
let selection_sort lst =
  let rec selection_sort' acc lst =
    match lst with 
    | [] -> acc
    | _ -> selection_sort' (acc @ [min_elt lst]) (remove (min_elt lst) lst)
  in 
  selection_sort' [] lst  

(* 2a *)
(* Modified version of the map function, except the index of the elements in list are affected. *)
let mapi f lst =
  let rec mapi' index f lst =
    match lst with
    | [] -> []
    | h::t -> f index h :: mapi' (index + 1) f t
  in 
  mapi' 0 f lst  

(* 2b *)
(* Returns a list of elements from lst whose index is a multiple of n *)
let every n lst = lst |> mapi (fun i x -> (i, x)) 
                      |> List.filter (fun (i, x) -> i mod n = 0) 
                      |> List.map (fun (i, x) -> x)

(* 3a *)
(* Short circuit version of fold_left, processes elements 
   from left to right but stops if a certain condition is met. *)
let rec fold_until f init lst =
  match lst with
  | [] -> init
  | h::t -> 
    match (f init h) with
    | None -> init
    | Some c ->  fold_until f c t

(* 3b *)
(* Helper function to check whether a number is negative or not.
   If yes, return none, else return sum of x and y. *)
let check_pos x y =
  if y < 0 then None
  else Some (x + y)  

(* Adds the sum of a list, but stops if a negative value occurs when processing list from left to right. *)
let sum_until_nonpos lst = fold_until check_pos 0 lst