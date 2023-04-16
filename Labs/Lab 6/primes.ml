module Primes = Map.Make(Int);; 

(* Converts each number in a list into a single int *)
let list_to_int lst =
  List.fold_right  (fun x acc -> x + acc * 10) lst 0

(* Sorts the digits in a number in descnding order *)
let sort_digits x =
  let rec sort_digits' x inc acc =
    if (inc == 6) then List.sort compare acc |> list_to_int else
      sort_digits' (x / 10) (inc + 1) (x mod 10::acc) 
  in
  sort_digits' x 0 [] 

(* Reads in data via stdin *)
let read_input () = 
  let rec read_input' acc =
    try
      let line = read_int() in
      read_input' (sort_digits line::acc)
    with
    | _ -> acc
  in read_input' []

(* Creates a dictionary map of permutations with the key being the permutation and value the frequency*)
let rec make_dictionary m lst =
  match lst with
  | [] -> m
  | h::t when (Primes.mem h m) -> make_dictionary (Primes.add h ((Primes.find h m) + 1) m) t
  | h::t -> make_dictionary (Primes.add h 1 m) t

(* Driver *)
let () = 
  let map = read_input() |>
  make_dictionary Primes.empty in
  Printf.printf "Largest set of 6 digit primes: %d\n" (Primes.fold (fun k v max -> if v > max then v else max) map 0)

 
  
