open Lambda_calculus_interpreter.Ast
open Lambda_calculus_interpreter.Eval

let () =
  let _fact =
    Let (
      "fact",
      Fix (Lambda ("g", Lambda ("x", IfZero (Variable "x", Constant 1, Multiplication (Variable "x", Application (Variable "g", Subtraction (Variable "x", Constant 1))))))),
      Application (Variable "fact", Constant 60))
  in
  let _fib =
    Let (
      "fib",
      Fix (Lambda ("g", Lambda ("x",
        IfZero (Subtraction (Variable "x", Constant 1), Constant 1, IfZero (Variable "x", Constant 1, Addition (Application (Variable "g", Subtraction (Variable "x",  Constant 1)), Application (Variable "g", Subtraction (Variable "x",  Constant 2)))))))),
      Application (Variable "fib", Constant 5)
    )
  in
  match eval_cps _fact [] (fun x -> x) with
  | Int x -> Printf.printf "%d\n" x
  | _ -> failwith "could not reach a constant value"
