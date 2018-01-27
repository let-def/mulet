module type SIGMA = sig
  include Map.OrderedType
  val empty : t
  val full : t
  val is_empty : t -> bool
  val is_full : t -> bool
  val is_subset_of : t -> t -> bool
  val compl : t -> t
  val inter : t -> t -> t
end

module Make(Sigma : SIGMA) (Label : Map.OrderedType) = struct

  type sigma = Sigma.t
  type label = Label.t

  module SigmaSet = Set.Make(Sigma)

  module Re : sig
    type t = private
      | Set     of sigma
      | Epsilon
      | Concat  of t * t
      | Closure of t
      | Or      of t * t
      | And     of t * t
      | Not     of t
      | Label   of label

    val compare : t -> t -> int
    val compare_modulo_labels : t -> t -> int

    val empty : t
    val is_empty : t -> bool
    val epsilon : t
    val star : t -> t
    val set : sigma -> t
    val ( ^. ) : t -> t -> t
    val ( &. ) : t -> t -> t
    val ( |. ) : t -> t -> t
    val compl : t -> t
    val label : label -> t

    val delta : sigma -> t -> t
    val labels : t -> label list
    val classes : t -> SigmaSet.t
  end = struct
    type t =
      | Set     of sigma
      | Epsilon
      | Concat  of t * t
      | Closure of t
      | Or      of t * t
      | And     of t * t
      | Not     of t
      | Label   of label

    let empty = Set Sigma.empty

    let not_empty = Not empty

    let is_empty = function
      | Set x -> Sigma.is_empty x
      | _ -> false

    let is_full = function
      | Not (Set x) -> Sigma.is_empty x
      | Closure (Set x) -> Sigma.is_full x
      | _ -> false

    let compare_tags = compare

    let rec compare x y =
      if x == y then 0
      else match x, y with
        | Set xs, Set ys -> Sigma.compare xs ys
        | Epsilon, Epsilon -> 0
        | Concat (x1, x2) , Concat (y1, y2)
        | Or     (x1, x2) , Or     (y1, y2)
        | And    (x1, x2) , And    (y1, y2) ->
          begin match compare x1 y1 with
            | 0 -> compare x2 y2
            | n -> n
          end
        | Closure x, Closure y -> compare x y
        | Not x, Not y -> compare x y
        | Label x, Label y -> Label.compare x y
        | ( Set _ | Epsilon | Concat _ | Closure _
          | Or _ | And _ | Not _ | Label _), _ ->
          compare_tags x y

    type ord = Lt | Eq | Gt

    let ord x y =
      match compare x y with
      | 0 -> Eq
      | n when n < 0 -> Lt
      | _ -> Gt

    let epsilon  = Epsilon
    let set s    = Set s

    let re_and a b =
      if is_empty a || is_empty b then empty
      else if is_full a then b
      else if is_full b then a
      else match ord a b with
        | Lt -> And (a, b)
        | Eq -> a
        | Gt -> And (b, a)

    let rec (&.) a b = match a with
      | And (a0, a1) -> re_and a0 (a1 &. b)
      | _ -> re_and a b

    let re_or a b =
      if is_full a || is_full b then not_empty
      else if is_empty a then b
      else if is_empty b then a
      else match ord a b with
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
      | Closure _ | Epsilon | Label _ as r -> r
      | x -> if is_empty x then Epsilon else Closure x

    let rec nullable = function
      | Epsilon -> true
      | Set _ -> false
      | Concat (a, b) | And (a, b) -> (nullable a) && (nullable b)
      | Or (a, b) -> (nullable a) || (nullable b)
      | Not x -> not (nullable x)
      | Closure _ -> true
      | Label _ -> true

    let delta x re =
      let rec delta = function
        | Set xs when Sigma.is_subset_of x xs -> epsilon
        | Set _ | Epsilon -> empty
        | Concat (r, s) when nullable r -> (delta r ^. s) |. delta s
        | Concat (r, s)   -> (delta r ^. s)
        | Closure r as rs -> (delta r ^. rs)
        | Or (r, s)       -> (delta r |. delta s)
        | And (r, s)      -> (delta r &. delta s)
        | Not r           -> compl (delta r)
        | Label _         -> empty
      in
      delta re

    let rec labels acc = function
      | Set _ | Epsilon -> acc
      | Concat (r, s) when not (nullable r) ->
        labels acc r
      | Closure r ->
        labels acc r
      | Concat (r, s) | Or (r, s) | And (r, s) ->
        labels (labels acc r) s
      | Not _ -> acc
      | Label label -> label :: acc

    let labels re = labels [] re

    let label l = Label l

    let rec compare_modulo_labels x y =
      if x == y then 0
      else match x, y with
        | Set xs, Set ys -> Sigma.compare xs ys
        | Epsilon, Epsilon -> 0
        | Concat (x1, x2) , Concat (y1, y2) ->
          begin match compare_modulo_labels x1 y1 with
            | 0 when nullable x1 ->
              compare_modulo_labels x2 y2
            | 0 ->
              compare x2 y2
            | n -> n
          end
        | Or  (x1, x2) , Or  (y1, y2)
        | And (x1, x2) , And (y1, y2) ->
          begin match compare_modulo_labels x1 y1 with
            | 0 -> compare_modulo_labels x2 y2
            | n -> n
          end
        | Closure x, Closure y -> compare_modulo_labels x y
        | Not x, Not y -> compare x y
        | Label x, Label y -> Label.compare x y
        | (Set _ | Epsilon | Concat _ | Or _ | And _ | Closure _ | Not _ | Label _), _ ->
          compare_tags x y

    let c_inter xs ys =
      SigmaSet.fold (fun x acc ->
          SigmaSet.fold (fun y acc ->
              SigmaSet.add (Sigma.inter x y) acc
            ) ys acc
        ) xs SigmaSet.empty

    let full = SigmaSet.singleton Sigma.full

    let rec classes = function
      | Epsilon | Label _ -> full
      | Set s -> SigmaSet.add (Sigma.compl s) (SigmaSet.singleton s)
      | Concat (r, s) when not (nullable r) -> classes r
      | Concat (r, s) | Or (r, s) | And (r, s) -> c_inter (classes r) (classes s)
      | Closure r -> classes r
      | Not r -> classes r
  end

  module DFA = Map.Make(struct
      type t = Re.t
      let compare = Re.compare_modulo_labels
    end)

  type transition = sigma * Re.t
  type dfa = transition DFA.t

  let rec make_dfa dfa = function
    | [] -> dfa
    | x :: todo when DFA.mem x dfa -> make_dfa dfa todo
    | x :: todo ->
      let class_delta sigma acc = (sigma, Re.delta sigma x) :: acc in
      let transitions = SigmaSet.fold class_delta (Re.classes x) [] in
      let dfa = DFA.add x transitions dfa in
      let add_todo todo (_, vec) = vec :: todo in
      let todo = List.fold_left add_todo todo transitions in
      make_dfa dfa todo
end

module Chars : sig
  include SIGMA
  val of_list : char list -> t
  val to_list : t -> char list
end = struct
  type t = string
  let empty = ""
  let full = String.make 32 '\xFF'
  let is_empty t = t = empty
  let is_full t = t = full
  let is_subset_of t1 t2 =
    let l1 = String.length t1 and l2 = String.length t2 in
    l1 <= l2 && (
      try
        for i = 0 to l1 - 1 do
          let c1 = Char.code t1.[i] and c2 = Char.code t2.[i] in
          if c1 land c2 <> c1 then raise Exit
        done;
        true
      with Exit -> false
    )

  let pack t =
    let len = String.length t in
    let last = ref len  in
    while !last > 0 && t.[!last - 1] = '\x00' do decr last done;
    if !last < len
    then String.sub t 0 !last
    else t

  let compl t =
    let s' =
      String.map (fun c -> Char.chr (0xFF land lnot (Char.code c))) t
    in
    let len = String.length s' in
    if len < 32 then
      s' ^ String.make (32 - len) '\xFF'
    else pack t

  let inter t1 t2 =
    let len = min (String.length t1) (String.length t2) in
    pack (String.init len (fun i ->
        let c1 = Char.code t1.[i] and c2 = Char.code t2.[i] in
        Char.chr (c1 land c2)
      ))

  let compare = String.compare

  let of_list = function
    | [] -> empty
    | cs ->
      let len = (Char.code (List.fold_left max '\000' cs) + 7) / 8 in
      let b = Bytes.make len '\000' in
      List.iter (fun c ->
          let c = Char.code c in
          Bytes.set b (c / 8)
            (Char.chr (Char.code (Bytes.get b (c / 8)) lor (c land 7)))
        ) cs;
      Bytes.unsafe_to_string b

  let to_list t =
    let r = ref [] in
    for i = String.length t - 1 downto 0 do
      let c = Char.code t.[i] in
      for j = 7 downto 0 do
        if c land (1 lsl j) <> 0 then
          r := Char.chr (i * 8 + j) :: !r
      done
    done ;
    !r
end
