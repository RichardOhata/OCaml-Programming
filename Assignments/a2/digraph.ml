type edge = string * string * int 
type t = edge list

exception Invalid

(* Checks if a pair of vertexes already exists in the graph to not cause conflicts *)
let rec exists vertex_pair acc = 
  match acc with 
  | [] -> false 
  | (x,y,z)::t when vertex_pair = (x,y) -> true
  | h::t -> exists vertex_pair t

(* Parses each line in the data file as an edge *)
let rec parse_data ic acc =
  try 
    let line = input_line ic in 
    try 
      let tuple = Scanf.sscanf line "%s %s %d" (fun x y z -> (x, y, z)) in
      match tuple with 
      | (x, y, z) when x = y -> raise Invalid
      | (x, y, z) when exists (x,y) acc -> raise Invalid
      | _ -> parse_data ic (tuple::acc)
    with 
    | _ -> []
  with 
  | _ -> List.rev acc

(* Reads digraph from file *)
let read_graph file_name =  
  try 
    let ic = open_in file_name in 
    parse_data ic [] 
  with
  | Sys_error _ ->
    Printf.fprintf stderr "Unable to open file: '%s'\n" file_name;
    []

(* Compares vertexes between edges *)
let compare_edges (x1, y1, z1) (x2, y2, z2) =
  let x = compare x1 x2 in
  if x != 0 then x else
  compare y1 y2

(* Returns a sorted list of all edges in the digraph *)
let edges digraph = 
  List.sort (compare_edges) digraph

(* Returns a list of all verticies *)
let rec get_vertices digraph acc= 
  match digraph with 
  | [] -> acc
  | (x,y,z)::t -> get_vertices t acc @ [x] @ [y]
  
(* Returns list of all distinct verticies *)
let vertices digraph = 
  List.sort_uniq (compare) (get_vertices digraph [])

(* Tests whether a given string is a vertex of the digraph *)
let is_vertex vertex digraph = 
  if (List.mem vertex (vertices digraph)) then true 
  else false