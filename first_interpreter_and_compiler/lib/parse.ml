let parse (s : string) : Interpreter.expr option =
  try
    let lexbuf = Lexing.from_string s in
    try
      Some(Parser.prog Lexer.read lexbuf)
      with
      | Parsing.Parse_error ->
        prerr_endline "Syntax error!";
        None
  with
  | _ ->
    prerr_endline "Lexing Error!";
    None
