(* 1. *)
(* Square roots a number and converts it into an int. *)
let int_sqrt n =
  n |>
  float_of_int |>
  sqrt |>
  int_of_float

(* Returns a list of all primes up to n *)
let primes n = 
  let rec primes' max inc acc =
      if (inc > max) then acc else (
        primes' max (inc+1) (List.filter (fun x -> x mod inc <> 0 || x == inc) acc)
      )
  in
  primes' (int_sqrt n) 2 (List.init (n-1) ((+) 2))

    
(* 2. *)
(* Filters out numbrers in a list that are not 6 digits *)
let rec onlySixDigits lst =
  match lst with
  | [] -> []
  | h::t when (h |> string_of_int |> String.length == 6) -> h::t
  | h::t -> onlySixDigits t 

(* Converts all integers in a list into a single number *)
let listToInt lst =
  List.fold_right  (fun x acc -> x + acc * 10) lst 0

(* Sorts the digits in a number in descending order *)
let sortDigits x = 
  let rec sortDigits' x inc acc = 
    if (inc == 6) then (List.sort compare acc) |> listToInt else
      sortDigits' (x / 10) (inc+1) (x mod 10::acc) 
  in 
  sortDigits' x 0 []

(* Finds the frequency of each permutation and then returns the largest *)
let countFrequency arr =
  let result = Array.make (Array.length arr) 0 in
  let visited = Array.make (Array.length arr) false in
    for i = 0 to Array.length arr - 1 do
      if (visited.(i) = false) then
        begin
        let counter = ref 1 in
        for j = i + 1 to Array.length arr - 1 do
          if (arr.(i) == arr.(j)) then
            begin
              visited.(j) <- true;
              incr counter;
              result.(i) <- !counter;
            end
        done;
        end
    done;
  Array.fold_left max 0 result
          
(* Finds the size of the largest set of 6 digit prime that are permutations of one another*)
let permutation_size () = 
  let primeArray = 999999 
  |> primes 
  |> onlySixDigits 
  |> Array.of_list 
  |> Array.map (sortDigits)
  |> countFrequency in
  primeArray



