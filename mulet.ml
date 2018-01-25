type ord =
  | Lt
  | Eq
  | Gt

module Sigma = struct
  type t = char

  type ts =
    | Fin of t list
    | Cofin of t list

  let all = Cofin []
  let empty = Fin []

  let is_empty = function
    | Fin [] -> true
    | _ -> false

  let is_all = function
    | Cofin [] -> true
    | _ -> false

  let mem x = function
    | Cofin xs -> not (List.mem x xs)
    | Fin xs -> List.mem x xs

  let compl = function
    | Fin xs -> Cofin xs
    | Cofin xs -> Fin xs

  let inter a b = match a, b with
    | Fin xs, Fin ys ->
      Fin (List.filter (fun x -> List.mem x ys) xs)
    | Fin xs, Cofin ys | Cofin ys, Fin xs ->
      Fin (List.filter (fun x -> not (List.mem x ys)) xs)
    | Cofin xs, Cofin ys ->
      Cofin (List.append xs ys)
end

module Re : sig
  type t = private
    | Set     of Sigma.ts
    | Epsilon
    | Concat  of t * t
    | Closure of t
    | Or      of t * t
    | And     of t * t
    | Not     of t

  val empty : t
  val is_empty : t -> bool
  val epsilon : t
  val star : t -> t
  val set : Sigma.ts -> t
  val ( ^. ) : t -> t -> t
  val ( &. ) : t -> t -> t
  val ( |. ) : t -> t -> t
  val compl : t -> t
end = struct
  type t =
    | Set     of Sigma.ts
    | Epsilon
    | Concat  of t * t
    | Closure of t
    | Or      of t * t
    | And     of t * t
    | Not     of t

  let empty = Set Sigma.empty

  let not_empty = Not empty

  let is_empty = function
    | Set x -> Sigma.is_empty x
    | _ -> false

  let is_not_empty = function
    | Not (Set x) -> Sigma.is_empty x
    | _ -> false

  let compare a b =
    let c = compare a b in
    if c = 0 then Eq else (if c < 0 then Lt else Gt)

  let epsilon  = Epsilon
  let set s    = Set s

  let re_and a b =
    if is_empty a || is_empty b then empty
    else if is_not_empty a then b
    else if is_not_empty b then a
    else match compare a b with
      | Lt -> And (a, b)
      | Eq -> a
      | Gt -> And (b, a)

  let rec (&.) a b = match a with
    | And (a0, a1) -> re_and a0 (a1 &. b)
    | _ -> re_and a b

  let re_or a b =
    if is_not_empty a || is_not_empty b then not_empty
    else if is_empty a then b
    else if is_empty b then a
    else match compare a b with
      | Lt -> Or (a, b)
      | Eq -> a
      | Gt -> Or (b, a)

  let rec (|.) a b = match a with
    | Or (a0, a1) -> re_or a0 (a1 |. b)
    | _ -> re_or a b

  let re_seq a b =
    if is_empty a || is_empty b then empty
    else match a, b with
      | Epsilon, x | x, Epsilon -> x
      | _ -> Concat (a, b)

  let rec (^.) a b = match a with
    | Concat (a0, a1) -> re_seq a0 (a1 ^. b)
    | _ -> re_seq a b

  let compl = function
    | Not x -> x
    | a -> Not a

  let star = function
    | Closure _ as r -> r
    | Epsilon -> Epsilon
    | x -> if is_empty x then Epsilon else Closure x
end

module Single = struct
  let rec nullable = function
    | Re.Epsilon -> true
    | Re.Set _ -> false
    | Re.Concat (a, b) | Re.And (a, b) -> (nullable a) && (nullable b)
    | Re.Or (a, b) -> (nullable a) || (nullable b)
    | Re.Not x -> not (nullable x)
    | Re.Closure _ -> true

  let nu x = if nullable x then Re.epsilon else Re.empty

  let delta x =
    let rec delta = function
      | Re.Set xs when Sigma.mem x xs -> Re.epsilon
      | Re.Set _ | Re.Epsilon -> Re.empty
      | Re.Concat (r, s) ->
        Re.((delta r ^. s) |. (nu r ^. delta s))
      | Re.Closure r as rs ->
        Re.(delta r ^. rs)
      | Re.Or (r, s) ->
        Re.(delta r |. delta s)
      | Re.And (r, s) ->
        Re.(delta r &. delta s)
      | Re.Not r -> Re.compl (delta r)
    in
    delta

  let deltas s r =
    let r = ref r in
    for i = 0 to String.length s - 1 do
      r := delta s.[i] !r
    done;
    !r

  let c_inter xs ys =
    List.fold_left (fun acc x ->
        List.fold_left (fun acc y -> Sigma.inter x y :: acc) acc ys
      ) [] xs

  let rec c = function
    | Re.Epsilon -> [Sigma.all]
    | Re.Set s -> [s; Sigma.compl s]
    | Re.Concat (r, s) when not (nullable r) -> c r
    | Re.Concat (r, s) | Re.Or (r, s) | Re.And (r, s) ->
      c_inter (c r) (c s)
    | Re.Closure r -> c r
    | Re.Not r -> c r
end

module Vector = struct
  type t = Re.t array

  let delta x t =
    Array.map (Single.delta x) t

  let deltas x t =
    if x = "" then t else
      Array.map (Single.deltas x) t

  let c t =
    let f acc r = Single.c_inter acc (Single.c r) in
    Array.fold_left f [Sigma.all] t
end

module DFA = struct
  let dfa r =
    let q0 = r in


end
