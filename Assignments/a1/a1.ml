(* Fisher-Yates shuffling algorithm, shuffle elements in an array *)
let shuffle a =
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done;
  a

(* Generates a population of random tours. There are size random tours and n cities *)
let populate n size = 
  let cities = Array.init n ((+) 0) in
    let rec populate' acc cities size = 
      if (size == 0) then acc else
        populate' (acc @ [cities |> shuffle |> Array.to_list]) cities (size-1)
      in
      populate'
      [] cities size


(* Parses the data *)
let rec parse_line line acc =
  if line= "" then 
    acc |> List.rev |> Array.of_list
else 
  Scanf.sscanf line "  %d %[^\n]" (fun n s -> parse_line s (n::acc))

(* Opens the file for reading, reads line by line *)
let read_file file_name = 
  try 
    let ic = open_in file_name in
    let rec read_file' ic acc =
      try 
        let line = input_line ic in 
        read_file' ic ((parse_line line [])::acc)
      with 
      | _ -> acc |> List.rev |> Array.of_list
    in read_file' ic []
  with
  | Sys_error _ -> 
      Printf.fprintf stderr "Unable to open file: '%s'\n" file_name;
      [||]
  
(* Reads data from a file to create the distance matrix *)
let read_distances file_name =
  read_file file_name

(* Returns the total distance (length) of a tour *)
let length distances tour  = 
  let head = List.hd tour in
  let rec length' distances tour acc =
    match tour with
    | h1::h2::t -> length' (distances) (h2::t) (acc + distances.(h1).(h2))
    | h::_ -> acc + distances.(h).(head)
    | [] -> acc
  in length' 
  distances tour 0

(* Groups the elements of a list into pairs *)
let pairs lst = 
  let rec pairs' lst acc =
    match lst with 
    | h1::h2::t -> pairs' t (acc @ [(h1,h2)])
    | _ -> acc
  in pairs' 
  lst []

(* Copies up to pos elements from one list into a new list *)
let rec copy pos t1 acc = 
  if (pos == 0) then acc else
    match t1 with
    | h::t -> copy (pos-1) t (acc @ [h])
    | [] -> acc

(* Crosses two tours *)
let cross pos t1 t2 = 
  let rec cross' t2 acc =
    match t2 with
    | [] -> acc
    | h::t when (List.mem h acc == false) -> cross' t (acc @ [h])
    | h::t -> cross' t acc
  in cross' 
  t2 (copy pos t1 [])


(* Performs crossover on a list of tours *)
let rec crossover tours cities acc = 
  match tours with 
  | (x,y)::t -> crossover t cities (acc @ [cross (Random.int (cities + 1)) x y] @ [cross (Random.int (cities + 1)) y x]) 
  | [] -> acc

(* Swaps two elements in a list specefied by index *)
let swap i j lst = 
  let first = List.nth lst i in 
  let second = List.nth lst j in 
  let rec swap' lst acc = 
    match lst with 
    | [] -> acc 
    | h::t when (h = first) -> swap' t (acc @ [second])
    | h::t when (h = second) -> swap' t (acc @ [first])
    | h::t -> swap' t (acc @ [h])
  in swap' 
  lst []

(* Performs mutation on the list of tours *)
let rec mutation tours cities prob acc = 
  let random_float = Random.float 1. in 
  match tours with
  | [] -> acc
  | h::t when (random_float < prob) -> mutation t cities prob (acc @ [swap (Random.int cities) (Random.int cities) h])
  | h::t -> mutation t cities prob (acc @ [h])


(* Performs the genetic algorithm *)
let rec algo niters prob distances pop =
  if (niters = 0) then pop |> List.sort (fun x y -> compare (length distances x) (length distances y)) |> List.hd |> length distances 
  else 
  (* Evaluation - Sorts the population in ascending order of tour length *)
  let sorted_population = List.sort (fun x y -> compare (length distances x) (length distances y)) pop in 

  (* Selection - Saves the fittest tour for the next generation and pairs the following tours in the population *)
  let first_tour = List.hd sorted_population in 
  let consecutive_tours = pairs (List.tl sorted_population) in 
  let num_of_cities = Array.length distances.(0) in

  (* Crossover - Performs crossover operations on each pair of tours *)
  let crossed_tours = crossover consecutive_tours num_of_cities [] in

  (* Mutation - Randomly performs mutation on each tours*)
  let mutated_tours = mutation crossed_tours num_of_cities prob [] in 
  algo (niters - 1) prob distances ([first_tour] @ mutated_tours) 
  

(* Runs the genetic algorithm *)
let run file pop_size prob niters = 
  let distance_matrix = read_distances file in
  let population = populate (Array.length distance_matrix.(0)) pop_size in 
  algo niters prob distance_matrix population