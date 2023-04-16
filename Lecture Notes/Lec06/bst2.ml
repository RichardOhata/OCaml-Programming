(* version of bst.ml with short names *)
type 'a bstree = L | N of 'a node
and
'a node = {v: 'a; l: 'a bstree; r: 'a bstree}

let rec bst_insert ~cmp x t =
  match t with
  | L -> N {v = x; l = L; r = L}
  | N {v; l; r} when cmp x v < 0 ->
      N {v; l = bst_insert ~cmp x l; r}
  | N {v; l; r} when cmp x v > 0 ->
      N {v; l; r = bst_insert ~cmp x r}
  | _ -> t

let bst_of_list ~cmp l =
  List.fold_left (fun t x -> bst_insert ~cmp x t) L l
