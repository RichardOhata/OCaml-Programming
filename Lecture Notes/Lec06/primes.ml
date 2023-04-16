let int_sqrt n =
  n |>
  float_of_int |>
  sqrt |>
  int_of_float

(* returns list of primes less then n *)
let primes n =
  let is_prime = Array.make n true in
  for i = 2 to int_sqrt n do
    if is_prime.(i) then
      let j = ref i in
      while !j * i < n do
        is_prime.(!j * i) <- false;
        incr j
      done
  done;
  is_prime.(0) <- false;
  is_prime.(1) <- false;
  is_prime |>
  Array.to_list |>
  List.mapi (fun i x -> (i, x)) |>
  List.filter_map (fun (i, x) -> if x then Some i else None)


  

  