(* summing integers read from standard input until either an invalid integer
   is encountered or until end-of-file *)
let rec sum acc =
  try
    sum @@ Scanf.scanf " %d" (fun x -> acc + x)
  with
  | _ -> acc

let () =
  Printf.printf "%d\n" @@ sum 0
