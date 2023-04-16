(* Parses an individual line from the file, extracting words and storing it into a list which will be added to the main list *)
let rec parse_line acc line =
  if line="" then
    List.rev acc
  else
    Scanf.sscanf line "%s %[^\n]" (fun n s-> parse_line (n::acc) s)

(* Extracts all words from the file and stores it into a string list *)
let words () =
  let rec words' acc = 
  try
   let line = read_line () in
   words' (acc @ (parse_line [] line))
  with
  | _ -> acc
in words' []

(* Adds all intergers in the list while excluding words that are not integers *)
let sum acc x =
  try
      acc + int_of_string x
  with
  | _ -> acc + 0

(* Driver *)
let () = 
 let res = List.fold_left sum 0 (words()) in
 Printf.printf "%d" res