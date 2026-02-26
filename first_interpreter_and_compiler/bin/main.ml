open Simple_interpreter_and_compiler.Interpreter
open Simple_interpreter_and_compiler.Compiler

let () =
  let e =
    match Simple_interpreter_and_compiler.Parse.parse "1 + 2 * 3" with
    | Some e -> e
    | None -> failwith "syntax error"
  in
  let result_interpreter = eval e in
  let result_compiler = List .hd (transition ([], (compile e))) in
  Printf.printf "%d %d\n" result_interpreter result_compiler
