let parse (s : string) : Ast.term option =
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


let parse_from_file file_path =
  let read_lines file_path =
    let contents =
      In_channel.with_open_bin
        file_path
        In_channel.input_all
    in
    String.split_on_char '\n' contents
  in
  let lines = read_lines file_path in
  List.fold_left (fun acc l -> acc ^ l) "" lines |> parse
