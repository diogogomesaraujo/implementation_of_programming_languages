open Ast
open Subst

let rec eval e =
  match e with
  | Constant n -> Constant n
  | Variable _ -> failwith "free variable occurence"
  | Addition (e1, e2) ->
    (match (eval e1,eval e2) with
    | (Constant e1, Constant e2) -> Constant (e1 + e2)
    | _ -> failwith "tried to add terms that aren't constants")
  | Subtraction (e1, e2) ->
    (match (eval e1,eval e2) with
    | (Constant e1, Constant e2) -> Constant (e1 - e2)
    | _ -> failwith "tried to subtract terms that aren't constants")
  | Multiplication (e1, e2) ->
    (match (eval e1,eval e2) with
    | (Constant e1, Constant e2) -> Constant (e1 * e2)
    | _ -> failwith "tried to multiply terms that aren't constants")
  | Lambda (x, e1) -> Lambda (x, e1)
  | Application (e1, e2) ->
    (match eval e1  with
    | Lambda (x, e) -> eval (subst e x (eval e2))
    | _ -> failwith "tried to apply a term to another that is not a lambda function")
  | IfZero (e1, e2, e3) ->
    (match eval e1 with
    | Constant 0 -> eval e2
    | Constant _ -> eval e3
    | _ -> failwith "the term can't be compared to zero because it's not a constant")
  | Fix e1 ->
    (match eval e1 with
    | Lambda (x, e) -> eval (subst e x (Fix e1))
    | _ -> failwith "tried to apply a term to another that is not a lambda function in a recursive call")
  | Let (x, e1, e2) -> eval (subst e2 x (eval e1))
