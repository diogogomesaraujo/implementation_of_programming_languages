(** Module that compiles the extended λ-Calculus to SECD-machine instructions.*)

open Ast
open Exn
open Secd
open Lookup

(** [compile e sym] compiles a term recursively into SECD-machine code.
It receives a term [e], as well as a symbol table [sym] that stores variable names at compile time.*)
let rec compile e sym =
  match e with
  | Constant n -> [LDC n]

  | Variable x ->
    let value = lookup x sym in
    [LD (value)]

  | Addition (e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 sym in
    c1 @ c2 @ [ADD]

  | Subtraction (e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 sym in
    c1 @ c2 @ [SUB]

  | Multiplication (e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 sym in
    c1 @ c2 @ [MUL]

  | Division (e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 sym in
    c1 @ c2 @ [DIV]

  | Lambda (x, e) ->
    let c = compile e (x::sym) in
    [LDF (c @ [RTN])]

  | Application (e1, e2) ->
    (compile e1 sym) @
    (compile e2 sym) @ [AP]

  | IfZero (e1, e2, e3) ->
    let c2 = (compile e2 sym) @ [JOIN] in
    let c3 = (compile e3 sym) @ [JOIN] in
    (compile e1 sym) @ [SEL (c2, c3)]

  | Let (x, e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 (x::sym) in
    c1 @ [AA] @ c2

  | Fix (Lambda(f, Lambda(x, e))) ->
    let c = compile e (x::f::sym) in
    [LDFR (c @ [RTN])]

  | _ -> raise (Exn (Semantic, "failed to compile the program"))
