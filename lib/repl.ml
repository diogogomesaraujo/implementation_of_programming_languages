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
    Printf.printf "\n";
  done

(** [read_term ()] reads lines from the stdin until [";;"] expression appears.*)
let read_term () =
  let append a b =  a ^ b in
  Printf.printf "> ";
  let rec read_term_rec acc =
    let line = read_line () in
    match String.ends_with ~suffix: ";;" line with
    | true -> append acc (String.sub line 0 (String.length line - 2))
    | false -> append acc line |> read_term_rec
  in
  read_term_rec ""

(** [repl_rec ()] interacts with the user and evaluates programs read from the stdin.*)
let rec repl_rec () =
  try
    let e =
      try
         read_term () |> parse
      with _ -> raise (Exn (Runtime, "couldn't parse user input"))
    in

    match compile e [] |> run with
    | Int x ->
      Printf.printf "< %d\n\n" x; repl_rec ()
    | _ -> raise (Exn (Runtime, "couldn't reach an integer value"))
  with Exn (t, e) ->
    print_exn (t, e);
    repl_rec ()

(** [repl] executes the interactive environment.*)
let repl () =
  clear ();
  Printf.printf "welcome to fun's repl! type your programs bellow.\n\n";
  repl_rec ()
