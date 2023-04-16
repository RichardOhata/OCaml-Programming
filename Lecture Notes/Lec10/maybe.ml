let ( >>= ) m f =
  match m with
  | None -> None
  | Some m' -> f m'

let return x = Some x

let ( <$> ) f m =
  match m with
  | None -> None
  | Some m' -> Some (f m')

let ( <*> ) f m =
  match f with
  | None -> None
  | Some f' -> f' <$> m

let ( *> ) a b = (fun _ x -> x) <$> a <*> b

let ( <* ) a b = (fun x _ -> x) <$> a <*> b
