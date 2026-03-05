open Lambda_calculus_interpreter.Ast
open Lambda_calculus_interpreter.Eval

let () =
  let fact_5 =
    Let (
      "fact",
      Fix (Lambda ("g", Lambda ("x", IfZero (Variable "x", Constant 1, Multiplication (Variable "x", Application (Variable "g", Subtraction (Variable "x", Constant 1))))))),
      Application (Variable "fact", Constant 5))
  in
  match eval fact_5 with
  | Constant x -> Printf.printf "%d\n" x
  | _ -> failwith "could not reach a constant value"
