(** Module that contains the functionalities to run a set of SECD-machine instructions.*)

open Secd
open Execute

(** [run i] runs the set of instructions [i] until it reaches a final value.*)
let run i =
  execute_t ([], [], i, [], StoreMap.empty)
