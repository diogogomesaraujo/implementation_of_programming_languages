open Fun.Compile
open Fun.Run
open Fun.Parse

let () =
  let fib = parse_from_file "examples/fib.fn" |> Option.get in
  match compile fib [] |> run with
  | Int x -> Printf.printf "%d\n" x
  | _ -> failwith "Couldn't reach an integer value."
