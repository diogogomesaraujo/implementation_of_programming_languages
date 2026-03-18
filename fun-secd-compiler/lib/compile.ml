open Ast
open Types
open Lookup

let rec compile e sym =
  match e with
  | Constant n -> [LDC n]
  | Variable x ->
    let value = lookup x sym in
    [LD (value)]
  | Addition (e1, e2) ->
    (compile e1 sym) @
    (compile e2 sym) @ [ADD]
  | Subtraction (e1, e2) ->
    (compile e1 sym) @
    (compile e2 sym) @ [SUB]
  | Multiplication (e1, e2) ->
    (compile e1 sym) @
    (compile e2 sym) @ [MUL]
  | Lambda (x, e) ->
    [LDF ((compile e (x::sym)) @ [RTN])]
  | Application (e1, e2) ->
    (compile e1 sym) @
    (compile e2 sym) @ [AP]
  | IfZero (e1, e2, e3) ->
    let c2 = (compile e2 sym) @ [JOIN] in
    let c3 = (compile e3 sym) @ [JOIN] in
    (compile e1 sym) @ [SEL (c2, c3)]
  | Let (x, e1, e2) ->
    compile ((Application(Lambda (x, e2), e1))) sym
  | Fix (Lambda(f, Lambda(x, e))) -> [LDFR ((compile e (x::f::sym)) @ [RTN])]
  | _ -> failwith "semantic error"
