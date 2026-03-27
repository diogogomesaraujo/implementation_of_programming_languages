(** Module that implements the interactive environment.*)

open Run
open Exn
open Parse
open Compile
open Terminal_size

(** [clear] prints [\n] for every row in the terminal.*)
let clear () =
  let r = match get_rows () with
    | Some r -> r
    | None -> raise (Exn (Runtime, "can't get terminal rows"))
  in
  for _ = 0 to r do
    Printf.printf "\n"
  done

(** [repl_rec ()] interacts with the user and evaluates programs read from the stdin.*)
let rec repl_rec () =
  Printf.printf "> ";
  try
    let e = read_line () |> parse in
    match compile e [] |> run with
    | Int x -> Printf.printf "< %d\n\n" x; repl_rec ()
    | _ -> raise (Exn (Runtime, "couldn't reach an integer value"))
  with Exn (t, e) ->
    print_exn (t, e);
    repl_rec ()

(** [repl] executes the interactive environment.*)
let repl () =
  clear ();
  Printf.printf "welcome to fun's repl! type your programs bellow.\n\n";
  repl_rec ()
