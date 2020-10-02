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

module type MONOID = sig
  type t
  val empty : t
  val append : t -> t -> t
end

module type LABEL = sig
  include Map.OrderedType
  include MONOID with type t := t
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
      | Label of label
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
    val label : label -> t
    val get_label : t -> label
  end

  module DFA : Map.S with type key = Re.t
  type transition = sigma * label * Re.t
  type dfa = transition list DFA.t

  val add_to_dfa : dfa -> Re.t list -> dfa
  val make_dfa : Re.t -> dfa
end

module Chars : sig
  include SIGMA
  val of_list : char list -> t
  val to_list : t -> char list
end
