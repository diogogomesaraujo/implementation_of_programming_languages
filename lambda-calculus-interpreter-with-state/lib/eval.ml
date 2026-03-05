open Ast
open Value
open Lookup

let rec eval e en =
  match e with
  | Constant n -> Int n
  | Variable v ->
    (match lookup en v with
    | Some v -> v
    | None -> failwith "free variable occurence")
  | Addition (e1, e2) ->
    (match (eval e1 en,eval e2 en) with
    | (Int e1, Int e2) -> Int (e1 + e2)
    | _ -> failwith "tried to add terms that aren't constants")
  | Subtraction (e1, e2) ->
    (match (eval e1 en,eval e2 en) with
    | (Int e1, Int e2) -> Int (e1 - e2)
    | _ -> failwith "tried to subtract terms that aren't constants")
  | Multiplication (e1, e2) ->
    (match (eval e1 en,eval e2 en) with
    | (Int e1, Int e2) -> Int (e1 * e2)
    | _ -> failwith "tried to multiply terms that aren't constants")
  | Lambda (x, e1) -> Closure (Lambda (x, e1), en)
  | Application (e1, e2) ->
    (match eval e1 en  with
    | Closure (Lambda (x, e), en1) -> eval e ((x, eval e2 en)::en1)
    | _ -> failwith "tried to apply a term to another that is not a lambda function")
  | IfZero (e1, e2, e3) ->
    (match eval e1 en with
    | Int 0 -> eval e2 en
    | Int _ -> eval e3 en
    | _ -> failwith "the term can't be compared to zero because it's not a constant")
  | Fix e1 ->
    (match eval e1 en with
    | Closure (Lambda (x, e), en) ->
      let rec v = Closure (e, (x, v)::en) in v
    | _ -> failwith "can't apply non closure value to a fixed point"
    )
  | Let (x, e1, e2) ->
    let v = eval e1 en in
    eval e2 ((x, v)::en)
