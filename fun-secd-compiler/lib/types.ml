open Ast

type env = value list

and value =
  | Int of int
  | Address of addr

and instr =
  | HALT
  | LDC of int
  | LD of int
  | ADD
  | SUB
  | MUL
  | SEL of instr list * instr list
  | JOIN
  | LDF of instr list
  | LDFR of instr list
  | AP
  | LDRF
  | RTN

and code = instr list

and closure = code * env

and addr = int

and stack = value list

module Addr = struct
  type t = addr
  let compare = compare
end

module StoreMap = Map.Make(Addr)

let next m = StoreMap.cardinal m + 1
let add k v m = StoreMap.add k v m
let get k m = StoreMap.find k m

type store = closure StoreMap.t

and dump = (stack * env * code) list

and sym = identity list

type conf = stack * env * code * dump * store
