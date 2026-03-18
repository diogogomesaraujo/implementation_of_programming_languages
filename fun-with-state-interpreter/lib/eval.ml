open Ast
open Value
open Lookup

let rec eval e en =
  match e with
  | Constant n -> Int n
  | Variable x ->
    (match lookup en x with
    | Some x -> x
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
    | _ -> failwith "can't apply non-closure value to a fixed point"
    )
  | Let (x, e1, e2) ->
    let v = eval e1 en in
    eval e2 ((x, v)::en)

let rec eval_cps e en c =
  match e with
  | Constant n -> c (Int n)
  | Variable v ->
    (match lookup en v with
    | Some v -> c v
    | None -> failwith "free variable occurence")
  | Addition (e1, e2) ->
    eval_cps e1 en (fun v1 ->
      eval_cps e2 en (fun v2 ->
        (match (v1, v2) with
        | (Int v1, Int v2) -> c (Int (v1 + v2))
        | _ -> failwith "can't add values that aren't integers"
        )))
  | Subtraction (e1, e2) ->
    eval_cps e1 en (fun v1 ->
      eval_cps e2 en (fun v2 ->
        (match (v1, v2) with
        | (Int v1, Int v2) -> c (Int (v1 - v2))
        | _ -> failwith "can't subtract values that aren't integers"
        )))
  | Multiplication (e1, e2) ->
    eval_cps e1 en (fun v1 ->
      eval_cps e2 en (fun v2 ->
        (match (v1, v2) with
        | (Int v1, Int v2) -> c (Int (v1 * v2))
        | _ -> failwith "can't multiplication values that aren't integers"
        )))
  | Lambda (x, e1) -> c (Closure (Lambda (x, e1), en))
  | Application (e1, e2) ->
    eval_cps e1 en (fun v1 ->
      eval_cps e2 en (fun v2 ->
        (match v1 with
        | Closure (Lambda (x, e), en1) -> eval_cps e ((x, v2)::en1) c
        | _ -> failwith "tried to apply a term to another that is not a lambda function")
        ))
  | IfZero (e1, e2, e3) ->
      eval_cps e1 en (fun v1 ->
            (match v1 with
            | Int 0 -> eval_cps e2 en c
            | Int _ -> eval_cps e3 en c
            | _ -> failwith "the term can't be compared to zero because it's not a constant")
          )
  | Fix e1 ->
    eval_cps e1 en (fun v1 ->
      (match v1 with
      | Closure (Lambda (x, e), en) ->
        let rec v = Closure (e, (x, v)::en) in c v
      | _ -> failwith "can't apply non-closure value to a fixed point"
      )
    )
  | Let (x, e1, e2) ->
    eval_cps e1 en (fun v1 ->
      eval_cps e2 ((x, v1)::en) c
    )
