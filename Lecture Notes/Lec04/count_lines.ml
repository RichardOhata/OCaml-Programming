(* Program to count the number of lines read from stdin *)
let rec count n =
  try
    ignore @@ read_line ();
    count (n + 1)
  with
  | _ -> n

let () =
  print_int @@ count 0;
  print_newline ()
