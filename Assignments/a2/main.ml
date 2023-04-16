open Digraph

(* Initalizes the list of distances *)
let rec distances_init verticies source acc = 
  match verticies with 
  | [] -> acc 
  | h::t when h = source -> distances_init t source (acc @ [(h, 0)])
  | h::t -> distances_init t source (acc @ [(h, Int.max_int)])
  
(* Initalizes the list of predecessors *)
let rec predecessor_init verticies acc =
  match verticies with 
  | [] -> acc 
  | h::t -> predecessor_init t (acc @ [(h, "")])

(* Returns a specfied distance in the list of distances *)
let rec get_distance distances vertex = 
  match distances with 
  | [] -> 0
  | (x, y)::t when x = vertex -> y 
  | (x, y)::t -> get_distance t vertex

(* Sets a new distance in the list of distances *)
let rec set_distance distances vertex new_weight acc = 
  match distances with 
  | [] -> acc 
  | (x, y)::t when x = vertex -> set_distance t vertex new_weight (acc @ [(x, new_weight)]) 
  | (x, y)::t -> set_distance t vertex new_weight (acc @ [(x,y)])

(* Sets a new predecessor in the list of predecessors *)
let rec set_predecessor predecessor vertex new_vertex acc =
  match predecessor with 
  | [] -> acc 
  | (x, y)::t when x = vertex -> set_predecessor t vertex new_vertex (acc @ [(x, new_vertex)])
  | (x, y)::t -> set_predecessor t vertex new_vertex (acc @ [(x,y)]) 

(* Returns the final result from the algorithm *)
let rec result predecessor distance acc = 
  match predecessor, distance with 
  | [], [] -> acc
  | (x,y)::t, (x', y')::t' -> result t t' (acc @ [(x, (y', y))]) 
  | _,_ -> acc

(* Checks if there is a negative weight cycle in the digraph *)
let rec negative_weight_cycle digraph distance predecessor = 
  match digraph with 
  | [] -> result predecessor distance [] 
  | (u, v, w)::t when (get_distance distance u != Int.max_int 
      && get_distance distance u + w < get_distance distance v) -> raise (failwith "Graph contains a negative weight cycle")
  | (u, v, w)::t -> negative_weight_cycle t distance predecessor

(* Helper function for repeatedly relaxing the edges *)
let rec relax' digraph original_digraph iter distance predecessor = 
  if (iter = 0) then negative_weight_cycle original_digraph distance predecessor else 
    match digraph with
    | [] -> relax' original_digraph original_digraph (iter-1) distance predecessor
    | (u, v, w)::t when (get_distance distance u != Int.max_int 
      && get_distance distance u + w < get_distance distance v) -> relax' t original_digraph iter 
      (set_distance distance v (get_distance distance u + w) []) (set_predecessor predecessor v u []) (* distance[v] = distance[u] + w && predecessor[v] = u*)
    | (u, v, w)::t -> relax' t original_digraph iter distance predecessor
  
(* Repeatedly relaxes the edges in the digraph *)
let relax digraph iter distance predecessor= 
  let original_digraph = digraph in 
  relax' digraph original_digraph (iter-1) distance predecessor

(* Performs the bellman ford algorithm *)
let bellman_ford source digraph =
  if (Digraph.is_vertex source digraph == false) then raise (failwith "Source vertex doesn't exist in digraph")
  else
  let distances = distances_init (Digraph.vertices digraph) source [] in 
  let predecessor = predecessor_init (Digraph.vertices digraph) [] in 
  relax (Digraph.edges digraph) (List.length (Digraph.vertices digraph)) distances predecessor