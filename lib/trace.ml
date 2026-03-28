(** Module that traces every step of the execution.*)

open Secd
open Execute

module Trace = struct
  type t = int * int * conf list
    [@@deriving show]

  (** [trace_config c] that constructs a list of the configurations in each step of the execution.
  It also returns the max stack and dump sizes.*)
  let config c =
    let rec trace_rec c l s_size d_size =
      match execute c with
      | (_::[], _, [], _, _) -> (s_size, d_size, l)
      | c' ->
        let (s, _, _, d, _) = c' in
        trace_rec
          c'
          (l @ [c])
          (max (List.length s) s_size)
          (max (List.length d) d_size)
    in
    trace_rec c [] 0 0

  (** [trace i] receives an instruction list [i] and calls [trace_config] for an initial configuration.*)
  let instr i =
    config ([], [], i, [], StoreMap.empty)
end
