type table
type index = private int

val follow :
  table -> index -> string -> offset:int ref -> len:int -> index

type negative_int = int

module Compiler() : sig
  type state
  type action = Goto of state | Custom of negative_int
  val state : unit -> state
  val transition : state -> char -> char -> action -> unit
  val compile : unit -> table * (state -> index)
end
