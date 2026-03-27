(** Module that implements the general exeption type.*)

(** Type that represents the possible exeption cases.*)
type exn_type =
  | Runtime
  | Semantic
  | Lex
  | Parse
  | Syntax
  | Compile

(** [show_exn_type t] converts an exeption type to [string].*)
let show_exn_type t =
  match t with
  | Runtime -> "runtime"
  | Semantic -> "semantic"
  | Lex -> "lexing"
  | Parse -> "parsing"
  | Syntax -> "syntax"
  | Compile -> "compile"


(** General exeption type.*)
exception Exn of exn_type * string

(** [print_exn (t, s)] prints an error to stdout.*)
let print_exn (t, s) = Printf.printf "< **%s error** %s.\n\n" (show_exn_type t) s
