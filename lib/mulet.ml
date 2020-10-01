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

module type LABEL = sig
  include Map.OrderedType
  val merge : t -> t -> t
end

module Make(Sigma : SIGMA) (Label : LABEL) = struct

  type sigma = Sigma.t
  type label = Label.t

  module SigmaSet = Set.Make(Sigma)

  module Re : sig
    type t = private
      | Set     of Sigma.t
      | Epsilon
      | Concat  of t * t
      | Closure of t
      | Or      of t * t
      | And     of t * t
      | Not     of t

    val compare : t -> t -> int

    val empty : t
    val is_empty : t -> bool
    val epsilon : t
    val star : t -> t
    val set : Sigma.t -> t
    val ( ^. ) : t -> t -> t
    val ( &. ) : t -> t -> t
    val ( |. ) : t -> t -> t
    val compl : t -> t
  end = struct
    type t =
      | Set     of Sigma.t
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
        | (Set _ | Epsilon | Concat _ | Closure _ | Or _ | And _ | Not _), _ ->
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
      else if is_not_empty a then b
      else if is_not_empty b then a
      else match ord a b with
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
      | Closure _ as r -> r
      | Epsilon -> Epsilon
      | x -> if is_empty x then Epsilon else Closure x
  end

  module ReMap = Map.Make(Re)

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
        | Re.Set xs when Sigma.is_subset_of x xs -> Re.epsilon
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

    let c_inter xs ys =
      SigmaSet.fold (fun x acc ->
          SigmaSet.fold (fun y acc ->
              SigmaSet.add (Sigma.inter x y) acc
            ) ys acc
        ) xs SigmaSet.empty

    let rec classes = function
      | Re.Epsilon -> SigmaSet.singleton Sigma.full
      | Re.Set s -> SigmaSet.add (Sigma.compl s) (SigmaSet.singleton s)
      | Re.Concat (r, s) when not (nullable r) -> classes r
      | Re.Concat (r, s) | Re.Or (r, s) | Re.And (r, s) -> c_inter (classes r) (classes s)
      | Re.Closure r -> classes r
      | Re.Not r -> classes r
  end

  module Vector = struct
    type t = Label.t ReMap.t

    let delta x t =
      let delta_label re label acc =
        let re' = Single.delta x re in
        let label' = match ReMap.find re' acc with
          | exception Not_found -> label
          | label' -> Label.merge label label'
        in
        if Re.is_empty re'
        then acc
        else ReMap.add re' label' acc
      in
      ReMap.fold delta_label ReMap.empty t

    let classes t =
      let f r _ acc = Single.c_inter acc (Single.classes r) in
      ReMap.fold f t (SigmaSet.singleton Sigma.full)

    let is_final t =
      ReMap.exists (fun re _ -> Single.nullable re) t

    let compare t1 t2 =
      ReMap.compare Label.compare t1 t2
  end

  module VecMap = Map.Make(Vector)

  type transitions = (sigma * label ReMap.t) list

  let rec make_dfa dfa = function
    | [] -> dfa
    | x :: todo when VecMap.mem x dfa -> make_dfa dfa todo
    | x :: todo ->
      let class_delta sigma acc = (sigma, Vector.delta sigma x) :: acc in
      let transitions = SigmaSet.fold class_delta (Vector.classes x) [] in
      let dfa = VecMap.add x transitions dfa in
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
