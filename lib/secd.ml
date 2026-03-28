(** Module that implement for SECD-machine code and virtual machine.*)

open Ast

(** Environment that maps variable names to values.*)
type env = value list
  [@@deriving show]

(** Value that can be either a constant [Int] or an [Address] for a variable's value position on the environment.*)
and value =
  | Int of int
  | Address of addr
  [@@deriving show]

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
  [@@deriving show]

(** Control list that contains the instructions to be executed.*)
and code = instr list
  [@@deriving show]

(** Pairing of a lambda-term and the environment state at the time of it's creation.*)
and closure = code * env
  [@@deriving show]

(** Identifier for a variable.*)
and addr = int
  [@@deriving show]

(** List of intermediate values produced during evaluation.*)
and stack = value list
  [@@deriving show]

(** Module that represents a map that will be used to define the [store].*)
module StoreMap = struct
  (** Module that represents an [addr] to be used in the [store].*)
  module Addr = struct
    type t = addr
    let compare = compare
    let show = string_of_int
  end

  (** Module that defines the store's map.*)
  module M = Map.Make(Addr)

  (** Type that represents the store's map.*)
  type t = closure M.t

  (** [next m] returns the number of key-value pairs in the [store].*)
  let next m = M.cardinal m + 1

  (** [add k v m] adds key-value pair [(k, v)]*)
  let add k v m = M.add k v m

  (** [get k m] gets the value of [k] in the store. It panics if [k] does not exist.*)
  let get k m = M.find k m

  (** [empty] is the empty store.*)
  let empty = M.empty

  (** [to_list] transforms a store into a list.*)
  let to_list m =
    M.fold (fun k v acc -> acc @ [(k, v)]) m []

  (** [show m] returns a memmory store in [string] format.*)
  let show m =
    let m = to_list m in
    let rec show_rec m =
      match m with
      | [] -> ""
      | (k, v)::[] ->
        Printf.sprintf "%s: %s"
          (show_addr k)
          (show_closure v)
      | (k, v)::tl ->
        Printf.sprintf "%s: %s, %s"
          (show_addr k)
          (show_closure v)
          (show_rec tl)
    in
    "{" ^ (m |> show_rec) ^ "}"

    (** [pp fmt m] is the memory store's pretty printer.*)
    let pp fmt m =
      Format.fprintf fmt "%s" (show m)
end

(** Memory store that holds closures.*)
type store = StoreMap.t
  [@@deriving show]

(** Saves the current state machine before entering a branch or function.*)
and dump = (stack * env * code) list
  [@@deriving show]

(** Symbol table for compilation purposes.*)
and sym = identity list
  [@@deriving show]

(** Configuration that represents the SECD-machine's state.*)
and conf = stack * env * code * dump * store
  [@@deriving show]
