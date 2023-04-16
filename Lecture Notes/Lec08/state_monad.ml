type ('a, 's) state = State of ('s -> 'a * 's)

let state f = State f

let run_state (State f) s = f s

let return x = State (fun s -> (x, s))

let ( >>= ) m f =
  State (fun s ->
    let (a, s') = run_state m s in
    run_state (f a) s')

let ( let* ) = ( >>= )

let ( >> ) m1 m2 = m1 >>= fun _ -> m2

let get = State (fun s -> (s, s))

let put new_state = State (fun _ -> ((), new_state))

let eval_state m s = fst @@ run_state m s

let exec_state m s = snd @@ run_state m s

(* 
   Ported from haskell state monad: https://wiki.haskell.org/State_Monad
   - go through string consisting of 'a's, 'b's and 'c's to produce a number
   - a 'c' toggles counting on and off; an 'a' add 1 and a 'b' subtracts 1
     when counting is on
*)
type game_score = int
type game_state = int * bool

let rec play_game' l =
  let* (score, on) = get in
  match l with
  | [] -> return score
  | x::xs ->
      (
      match x with
      | 'a' when on -> put (score + 1, on)
      | 'b' when on -> put (score - 1, on)
      | 'c' -> put (score, not on)
      | _ -> put (score, on)
      ) >> play_game' xs

let play_game s =
  play_game' @@ String.fold_right (fun x a -> x :: a) s []
