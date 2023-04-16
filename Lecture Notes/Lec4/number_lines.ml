(* Program to number and count lines read from stdin *)
let rec count n =
  try
    Printf.printf "%4d: %s\n" (n + 1) @@ read_line ();
    count (n + 1)
  with
  | _ -> n

let () =
  Printf.printf "Total number of lines = %d\n" @@ count 0
