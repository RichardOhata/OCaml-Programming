(* Program to sum integers, one on each line with no leading whitespace *)
let rec sum acc =
  try
    sum (acc + read_int ())
  with
  | _ -> acc

let () =
  print_int @@ sum 0;
  print_newline ()
