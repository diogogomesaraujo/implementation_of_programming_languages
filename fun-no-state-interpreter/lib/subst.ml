open Ast

let rec subst e1 v e2 =
  match e1 with
  | Variable x when x = v -> e2
  | Variable _ -> e1
  | Constant _ -> e1
  | Lambda (x, e_temp) when x <> v -> Lambda (x, subst e_temp v e2)
  | Lambda (x, e_temp) -> Lambda (x, e_temp)
  | Application (a, b) -> Application (subst a v e2, subst b v e2)
  | Addition (a, b) -> Addition (subst a v e2, subst b v e2)
  | Subtraction (a, b) -> Subtraction (subst a v e2, subst b v e2)
  | Multiplication (a, b) -> Multiplication (subst a v e2, subst b v e2)
  | Fix a -> Fix (subst a v e2)
  | Let (x, a, b) when x <> v -> Let (x, subst a v e2, subst b v e2)
  | Let (x, a, b) -> Let (x, a, b)
  | IfZero (a, b, c) -> IfZero (subst a v e2, subst b v e2, subst c v e2)
