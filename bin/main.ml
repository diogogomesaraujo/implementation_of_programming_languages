open Fun.Compile
open Fun.Run
open Fun.Parse
open Fun.Repl
open Fun.Exn

let usage_msg = "fun (--repl | <file>)"

let repl_flag = ref false

let input_file = ref ""

let anon_fun filename =
  input_file := filename

let spec_list =
  [("--repl", Arg.Set repl_flag, "Interactive environment")]

let () =
  Arg.parse spec_list anon_fun usage_msg;

  match !repl_flag = true with
  | true -> repl ();
  | false ->
    try
      let e =
        parse_from_file !input_file
      in

      match compile e [] |> run with
      | Int x -> Printf.printf "%d\n" x
      | _ -> raise (Exn (Runtime, "couldn't reach an integer value"))

    with Exn (t, e) -> print_exn (t, e)
