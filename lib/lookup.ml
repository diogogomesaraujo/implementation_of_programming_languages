(** Module that implements the symbol table lookup function.*)

open Exn

(** [lookup x l] searches in l for a value equal to x and panics if it is not able to find it.*)
let lookup x l =
  match List.find_index (fun e -> e = x) l with
  | Some v -> v
  | None -> raise (Exn (Compile, "failed due to free variable occurence"))
