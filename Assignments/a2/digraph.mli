  type t 
  type edge = string * string * int

  exception Invalid
  val read_graph : string -> t 
  val edges : t -> edge list 
  val vertices : t -> string list 

  val is_vertex : string -> t -> bool
  