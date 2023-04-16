(* [insert x lst] is the sorted list formed by putting [x] into [lst];
   the sorting order is specified by [cmp]; cmp a b return a negative int
   if a precedes b; it return 0 if a and b are equivalent; it returns a
   positive int if a follows b; requires: [lst] is already sorted *)
let rec insert ~cmp x lst =
  match lst with
  | [] -> [x]
  | y::ys when cmp x y > 0 -> y::insert ~cmp x ys
  | _ -> x::lst

(* [insertion_sort cmp lst] sorts [lst] in the order specified by [cmp] *)
let rec insertion_sort ~cmp lst =
  match lst with
  | [] -> []
  | x::xs -> insert ~cmp x @@ insertion_sort ~cmp xs


(* sorting list of pairs (x, y) in descending order of x & if the x's are
   the same, in ascending order of y *)
let sort_pairs l =
  let cmp (x1, y1) (x2, y2) =
    let x = compare x2 x1 in
    if x = 0 then compare y1 y2 else x
  in List.sort cmp l
