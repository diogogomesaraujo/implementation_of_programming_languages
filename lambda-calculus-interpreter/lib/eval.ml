open Ast
open Subst

let rec eval (e: term) : term =
  match e with
  | Constant n -> Constant n
  | Variable _ -> failwith "free variable occurence"
  | Addition (e1, e2) ->
      (match (eval e1,eval e2) with
    |   (Constant e1, Constant e2) -> Constant (e1 + e2)
    |   _ -> failwith "semantic error")
  | Subtraction (e1, e2) ->
      (match (eval e1,eval e2) with
    |   (Constant e1, Constant e2) -> Constant (e1 - e2)
    |   _ -> failwith "semantic error")
  | Multiplication (e1, e2) ->
      (match (eval e1,eval e2) with
    |   (Constant e1, Constant e2) -> Constant (e1 * e2)
    |   _ -> failwith "semantic error")
  | Lambda (x, e1) -> Lambda (x, e1)
  | Application (Lambda (x, e1), e2) -> eval (subst e1 x (eval e2))
  | Application (_, _) -> failwith "semantic error"
  | IfZero (e1, e2, e3) ->
      (match eval e1 with
      | Constant 0 -> eval e2
      | _ -> eval e3)
  | Fix e1 -> eval (Application (e1, Fix e1))
  | Let (x, e1, e2) -> eval (subst e2 x (eval e1))
