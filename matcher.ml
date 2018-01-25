type table = string
type index = int

external follow
  : table -> index -> string -> offset:int ref -> len:int -> index
  = "mulet_bsearch_match" [@@noalloc]

external put_int
  : bytes -> int -> int -> unit
  = "mulet_put_int"

type negative_int = int

module Compiler() : sig
  type state

  type action =
    | Goto of state
    | Custom of negative_int

  val state : unit -> state
  val transition : state -> char -> char -> action -> unit

  val compile : unit -> table * (state -> index)
end = struct
  type state = {
    mutable entrypoint : int;
    mutable transitions: (char * char * action) list;
  }

  and action =
    | Goto of state
    | Custom of negative_int

  let states = ref []

  let state () =
    let result = { entrypoint = -1; transitions = [] } in
    states := result :: !states;
    result

  let not_compiled state =
    if state.entrypoint <> -1 then
      invalid_arg "Compiler.transition: state has already been compiled"

  let transition state first last action =
    not_compiled state;
    if first > last then
      invalid_arg "Compiler.transition: invalid character range (first > last)";
    begin match action with
      | Goto state' -> not_compiled state;
      | Custom action ->
        if (action > 0) then
          invalid_arg "Compiler.transition: custom action index should <= 0"
    end;
    state.transitions <- (first, last, action) :: state.transitions

  let flush () =
    let result = !states in
    states := [];
    result

  let compute_offset_and_size transitions =
    (* Size is:
       1 byte for the number of transitions
       + 4 bytes per transition for the action
       + 2 bytes per transition for the dispatch table
       Transitions are stored immediately before the entrypoint.
    *)
    let len = List.length transitions in
    (4 * len, len * 6 + 1)

  let prepare_state state =
    let transitions = List.sort compare state.transitions in
    let rec validate previous = function
      | (first, last, action) :: rest ->
        if previous >= first then
          invalid_arg "Compiler.compile: \
                       overlapping transitions \
                       (multiple transition for the same character";
        validate last rest
      | [] -> ()
    in
    begin match transitions with
      | [] -> ()
      | (_, last, _) :: rest -> validate last rest
    end;
    state.transitions <- transitions

  let allocate_state offset state =
    (* Aligne on 4 bytes *)
    let offset = (offset + 3) land (lnot 3) in
    let (entrypoint, size) = compute_offset_and_size state.transitions in
    let entrypoint = offset + entrypoint in
    assert (entrypoint land 3 = 0);
    state.entrypoint <- entrypoint / 4;
    offset + size

  let get_entrypoint state =
    state.entrypoint

  let generate_actions table state =
    let count = List.length state.transitions in
    let start = state.entrypoint - count in
    assert (start >= 0);
    List.iteri (fun i (_, _, action) ->
        let v = match action with
          | Goto state -> state.entrypoint
          | Custom n -> n
        in
        put_int table (start + i) v
      ) state.transitions

  let generate_dispatch table state =
    let base = state.entrypoint * 4 in
    Bytes.set table base (Char.chr (List.length state.transitions));
    let gen_dispatch i (f1, l1, _) =
      Bytes.set table (base + 2 * i + 1) f1;
      Bytes.set table (base + 2 * i + 2) l1;
    in
    List.iteri gen_dispatch state.transitions

  let compile () =
    let states = flush () in
    List.iter prepare_state states;
    let total_size = List.fold_left allocate_state 0 states in
    let table = Bytes.create total_size in
    List.iter (generate_actions table) states;
    List.iter (generate_dispatch table) states;
    (Bytes.unsafe_to_string table, get_entrypoint)

end
