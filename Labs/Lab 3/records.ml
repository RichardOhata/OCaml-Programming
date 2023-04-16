(* Parses the records from each line from the input file. *)
let rec parse_data ic acc =
  try 
    let line = input_line ic in
    try 
      let tuple = Scanf.sscanf line "%s %s %d" (fun x y z -> (x, y, z)) in
      match tuple with
      | (_, _, x) when x < 0 -> parse_data ic acc
      | (_, _, x) when x > 100 -> parse_data ic acc
      | _ -> parse_data ic (tuple::acc)
    with
    | _ -> parse_data ic acc 
  with
  | _ -> List.rev acc

(* Sorts the list of records. *)
let sort_record lst =
  let cmp (x1, y1, z1) (x2, y2, z2) =
    let x = compare z2 z1 in
    if x != 0 then x else
    let y = compare y1 y2 in
    if y = 0 then compare x1 x2 else y 
    in List.sort cmp lst

(* Opens the file and calls parses & sorts methods *)
let read_file fname =
  try
    let ic = open_in fname in
    sort_record @@ parse_data ic [] 
  with
  | Sys_error _ ->
      Printf.fprintf stderr "Unable to open file: '%s'\n" fname;
      []

(* Prints the list of sorted records *)
let rec print_records =
  function
  | [] -> ()
  | (a, b, c) :: rest ->
    Printf.printf "%d %s %s\n" c b a;
    print_records rest

(* Driver *)
let () = 
  if Array.length Sys.argv > 1 then 
    print_records @@ read_file @@ Sys.argv.(1)   
  else
    Printf.fprintf stderr "Need to specify a file\n"