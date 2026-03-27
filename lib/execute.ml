(** Module that implements the evaluation of SECD-machine instructions.*)

open Exn
open Secd

(** [execute c] is the SECD virtual machine that evaluates a configuration [c]
based on the first instruction from the code.*)
let execute c =
  match c with
  (* Load a value from the current environment onto the stack.*)
  | (s, e, (LD i)::c, d, m) ->
    let value = List.nth e i in
    (value::s, e, c, d, m)

  (* Load a constant integer [n] onto the stack.*)
  | (s, e, (LDC n)::c, d, m) ->
    let value = Int n in
    (value::s, e, c, d, m)

  (* Pop the first two values in the stack and push their sum.*)
  | ((Int v1)::(Int v2)::s, e, ADD::c, d, m) ->
    let value = (Int (v2 + v1)) in
    (value::s, e, c, d, m)

  (* Pop the first two values in the stack and push their subtraction.*)
  | ((Int v1)::(Int v2)::s, e, SUB::c, d, m) ->
    let value = (Int (v2 - v1)) in
    (value::s, e, c, d, m)

  (* Pop the first two values in the stack and push their multiplication.*)
  | ((Int v1)::(Int v2)::s, e, MUL::c, d, m) ->
    let value = (Int (v2 * v1)) in
    (value::s, e, c, d, m)

  (* Pop the first two values in the stack and push their division.*)
  | ((Int v1)::(Int v2)::s, e, DIV::c, d, m) ->
    let value = (Int (v2 / v1)) in
    (value::s, e, c, d, m)

  (* Load a λ-abstraction as a closure onto the stack.*)
  | (s, e, (LDF c')::c, d, m) ->
    let address = next m in
    ((Address address)::s, e, c, d, add address (c', e) m)

  (* Moves a value from the temporary stack to the environment.*)
  | (v::s, e, AA::c, d, m) -> (s, v::e, c, d, m)

  (* Applies a closure to the current argument, saving the current state in the dump.*)
  | (v::(Address a)::s, e, AP::c, d, m) ->
    let (c', e') = get a m in
    ([], v::e', c', (s, e, c)::d, m)

  (* Returns from a function call and loads the previous state saved in the dump.*)
  | (v::_, _, RTN::_, (s',e',c')::d, m) ->
    (v::s', e', c', d, m)

  (* Implements conditional branching by selecting between two instruction sequences
  based on whether the top of the stack is zero.*)
  | ((Int 0)::s, e, SEL (c1, _)::c, d, m) ->
    (s, e, c1, ([], [], c)::d, m)
  | ((Int _)::s, e, SEL (_, c2)::c, d, m) ->
    (s, e, c2, ([], [], c)::d, m)

  (* Returns control to the instruction sequence saved in the dump after executing the branch.*)
  | (s, e, JOIN::_, (_, _, c')::d, m) ->
    (s, e, c', d, m)

  (* Does the same as the LDF but for recursive calls.*)
  | (s, e, (LDFR c')::c, d, m) ->
    let address = next m in
    let m' = add address (c', (Address address)::e) m in
    ((Address address)::s, e, c, d, m')

  (* If the term does not match any of the previous signatures, it is considered invalid.*)
  | _ -> raise (Exn (Semantic, "failed to execute the program"))

(** [execute_t c] evaluates the configuration [c] recursivly until the code and the stack are empty.*)
let rec execute_t c =
  match execute c with
  (* If there is only a final value on the stack and the instruction list is empty return the result of the computation.*)
  | (v::[], _, [], _, _) -> v

  (* Otherwise, recursivly call [execute_t] to the state until the previous condition is matched.*)
  | c' -> execute_t c'
