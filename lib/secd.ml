(** Module that implement for SECD-machine code and virtual machine.*)

open Ast

(** Environment that maps variable names to values.*)
type env = value list

(** Value that can be either a constant [Int] or an [Address] for a variable's value position on the environment.*)
and value =
  | Int of int
  | Address of addr

(** SECD-machine instructions based on Henderson's virtual machine for Lisp/Scheme.*)
and instr =
  | HALT
  | LDC of int
  | LD of int
  | ADD
  | SUB
  | MUL
  | DIV
  | SEL of instr list * instr list
  | JOIN
  | LDF of instr list
  | LDFR of instr list
  | AP
  | AA
  | LDRF
  | RTN

(** Control list that contains the instructions to be executed.*)
and code = instr list

(** Pairing of a lambda-term and the environment state at the time of it's creation.*)
and closure = code * env

(** Identifier for a variable.*)
and addr = int

(** List of intermediate values produced during evaluation.*)
and stack = value list

(** Module that represents an [addr] to be used in the [store].*)
module Addr = struct
  type t = addr
  let compare = compare
end

(** Module that represents a map that will be used to define the [store].*)
module StoreMap = Map.Make(Addr)

(** [next m] returns the number of key-value pairs in the [store].*)
let next m = StoreMap.cardinal m + 1

(** [add k v m] adds key-value pair [(k, v)]*)
let add k v m = StoreMap.add k v m

(** [get k m] gets the value of [k] in the store. It panics if [k] does not exist.*)
let get k m = StoreMap.find k m

(** Memory store that holds closures.*)
type store = closure StoreMap.t

(** Saves the current state machine before entering a branch or function.*)
and dump = (stack * env * code) list

(** Symbol table for compilation purposes.*)
and sym = identity list

(** Configuration that represents the SECD-machine's state.*)
and conf = stack * env * code * dump * store
