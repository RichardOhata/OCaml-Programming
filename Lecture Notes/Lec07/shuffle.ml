(* Fisher-Yates shuffling algorithm *)
let shuffle a =
  for i = Array.length a - 1 downto 1 do
    let j = Random.int (i + 1) in
    let tmp = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- tmp
  done;
  a

