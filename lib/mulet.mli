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

module Make (Sigma : SIGMA) (Label : LABEL) : sig
  type sigma = Sigma.t
  type label = Label.t

  module Re : sig
    type t = private
        Set of sigma
      | Epsilon
      | Concat of t * t
      | Closure of t
      | Or of t * t
      | And of t * t
      | Not of t
    val compare : t -> t -> int
    val empty : t
    val is_empty : t -> bool
    val epsilon : t
    val star : t -> t
    val set : sigma -> t
    val ( ^. ) : t -> t -> t
    val ( &. ) : t -> t -> t
    val ( |. ) : t -> t -> t
    val compl : t -> t
  end

  module ReMap : Map.S with type key = Re.t

  module Vector : sig
    type t = label ReMap.t
    val delta : sigma -> label ReMap.t -> label ReMap.t
    val is_final : 'a ReMap.t -> bool
    val compare : label ReMap.t -> label ReMap.t -> int
  end

  module VecMap : Map.S with type key = Vector.t

  type transitions = (sigma * label ReMap.t) list

  val make_dfa : transitions VecMap.t -> Vector.t list -> transitions VecMap.t
end

module Chars : sig
  include SIGMA
  val to_list : t -> char list
end
