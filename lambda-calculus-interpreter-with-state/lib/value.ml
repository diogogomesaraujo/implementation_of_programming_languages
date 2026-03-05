open Ast

type env = (identity * value) list
  [@@deriving show]
and value =
  | Int of int
  | Closure of term * env
  [@@deriving show]

type cont = value -> value
