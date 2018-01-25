type alphabet = char

type re =
  | Empty
  | Epsilon
  | Singleton of alphabet
  | Concat of re * re
  | Closure of re
  | Or of re * re
  | And of re * re
  | Not of re

let rec nu = function
  | Empty -> Empty
  | Singleton _ | Epsilon -> Epsilon
  | Concat (a, b) | And (a, b) -> min (nu a) (nu b)
  | Or (a, b) -> max (nu a) (nu b)
  | Not x -> (match nu x with Empty -> Epsilon | _ -> Empty)
  | Closure _ -> Epsilon

let delta x =
  let rec delta = function
    | Epsilon | Empty -> Empty
    | Singleton x' -> if x = x' then Epsilon else Empty
    | Concat (r, s) -> Or (Concat (delta r, s), Concat (nu r, delta s))
    | Closure r as rs -> Concat (delta r, rs)
    | Or (r, s) -> Or (delta r, delta s)
    | And (r, s) -> And (delta r, delta s)
    | Not r -> Not (delta r)
  in
  delta

let deltas s r =
  let r = ref r in
  for i = 0 to String.length s - 1 do
    r := delta s.[i] !r
  done;
  !r
