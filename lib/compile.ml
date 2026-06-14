(** Module that compiles the extended λ-Calculus to SECD-machine instructions.*)

open Ast
open Exn
open Secd
open Lookup

(** [compile_lambda l e sym] compiles a lambda term recursively until the argument list [l] has been fully consumed.*)
let rec compile_lambda l e sym =
  match l with
  | [] -> compile e sym
  | x::tl ->
    let c = compile_lambda tl e (x::sym)
    in [LDF (c @ [RTN])]

(** [compile_cases v l sym] compiles the branches of a [match] recursively.*)
and compile_cases v l sym =
  match l with
    | (t1, t2)::tl ->
      let c = compile t1 sym in
      let c1 = compile t2 sym @ [JOIN] in
      let c2 = compile_cases v tl sym @ [JOIN] in
      v @ c @ [EQ] @ [SEL (c1, c2)]
    | [] -> []

(** [compile e sym] compiles a term recursively into SECD-machine code.
It receives a term [e], as well as a symbol table [sym] that stores variable names at compile time.*)
and compile e sym =
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

  | Less (e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 sym in
    c1 @ c2 @ [LT]

  | Greater (e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 sym in
    c1 @ c2 @ [GT]

  | Equal (e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 sym in
    c1 @ c2 @ [EQ]

  | Different (e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 sym in
    c1 @ c2 @ [DF]

  | GreaterEqual (e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 sym in
    c1 @ c2 @ [GTE]

  | LessEqual (e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 sym in
    c1 @ c2 @ [LTE]

  | Lambda (l, e) ->
    compile_lambda l e sym

  | Application (e1, e2) ->
    compile e1 sym @
    compile e2 sym @ [AP]

  | IfZero (e1, e2, e3) ->
    let c2 = (compile e2 sym) @ [JOIN] in
    let c3 = (compile e3 sym) @ [JOIN] in
    compile e1 sym @ [SEL (c2, c3)]

  | And (e1, e2) ->
    compile e1 sym @ [SEL (compile e2 sym @ [SEL ([LDC 0], [LDC 1])], [LDC 1])]

  | Or (e1, e2) ->
    compile e1 sym @ [SEL ([LDC 0], compile e2 sym @ [SEL ([LDC 0], [LDC 1])])]

  | Match (x, l) ->
    compile_cases (compile x sym) l sym

  | Let (x, e1, e2) ->
    let c1 = compile e1 sym in
    let c2 = compile e2 (x::sym) in
    c1 @ [AA] @ c2

  | Def (f, l, e1, e2) ->
    let c1 = compile_lambda l e1 sym in
    let c2 = compile e2 (f::sym) in
    c1 @ [AA] @ c2

  | DefRec (f, l, e1, e2) ->
    let c1 = compile (Fix (Lambda (f::l, e1))) sym in
    let c2 = compile e2 (f::sym) in
    c1 @ [AA] @ c2

  | Fix Lambda(f::x::tl, e) ->
    let c = compile_lambda tl e (x::f::sym) in
    [LDFR (c @ [RTN])]

  | _ -> raise (Exn (Semantic, "failed to compile the program"))
