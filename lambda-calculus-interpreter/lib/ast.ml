type identity = string
  [@@deriving show]

type term =
  | Variable of identity
  | Lambda of identity * term
  | Application of term * term
  | Constant of int
  | IfZero of term * term * term
  | Let of identity * term * term
  | Addition of term * term
  | Subtraction of term * term
  | Multiplication of term * term
  | Fix of term
  [@@deriving show]
