type expr =
  | Num of int
  | Add of expr * expr
  | Mult of expr * expr
[@@deriving show]

let rec eval e =
  match e with
  | Num n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Mult (e1, e2) -> eval e1 * eval e2
