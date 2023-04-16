(* Program to number and count lines read from a file specified on the
   command-line *)
let rec count ic n =
  try
    Printf.printf "%4d: %s\n" (n + 1) @@ input_line ic;
    count ic (n + 1)
  with
  | _ -> n

let count_file fname =
  try
  let ic = open_in fname in
  count ic 0
  with
  | Sys_error _ ->
      Printf.fprintf stderr "Unable to open file: '%s'\n" fname;
      0

let () =
  if Array.length Sys.argv > 1 then
    Printf.printf "Total number of lines = %d\n" @@ count_file @@ Sys.argv.(1)
  else
    Printf.fprintf stderr "Need to specify a file\n"
