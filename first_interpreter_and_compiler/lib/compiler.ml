
type instr =
  | Push of int
  | Add
  | Mult
[@@deriving show]

type stack = int list
[@@deriving show]

type code = instr list
[@@deriving show]

type state = stack * code
[@@deriving show]

let rec compile e =
  match e with
  | Interpreter.Num n -> [Push n]
  | Interpreter.Add (e1, e2) -> compile e1 @ compile e2 @ [Add]
  | Interpreter.Mult (e1, e2) -> compile e1 @ compile e2 @ [Mult]

let rec transition s =
  match s with
  | (stack, []) -> stack
  | (stack, code) ->
    match code with
    | (Push n)::tl -> transition ((stack @ [n]), tl)
    | Add::code_tl ->
      (match stack with
      | e1::e2::stack_tl ->
        transition ((stack_tl @ [e1 + e2]), code_tl)
      | _ -> failwith "semantic error")
    | Mult::code_tl ->
      (match stack with
      | e1::e2::stack_tl ->
        transition ((stack_tl @ [e1 * e2]), code_tl)
      | _ -> failwith "semantic error")
    | _ -> failwith "empty"
