(** Module that implements the types that define the exetended λ-Calculus.*)

(** Type that represents a variable name.*)
type identity = string
  [@@deriving show]

type branch = term * term

(** Type that represents a term recognized by the compiler.*)
and term =
  | Variable of identity
  | Lambda of identity list * term
  | Application of term * term
  | Constant of int
  | IfZero of term * term * term
  | Match of term * branch list
  | Let of identity * term * term
  | Def of identity * identity list * term * term
  | DefRec of identity * identity list * term * term
  | Addition of term * term
  | Subtraction of term * term
  | Multiplication of term * term
  | Division of term * term
  | Less of term * term
  | Greater of term * term
  | Different of term * term
  | LessEqual of term * term
  | GreaterEqual of term * term
  | And of term * term
  | Or of term * term
  | Equal of term * term
  | Fix of term
  [@@deriving show]
