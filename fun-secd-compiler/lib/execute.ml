open Types

let execute c =
  match c with
  | (s, e, (LD i)::c, d, m) ->
    let value = List.nth e i in
    (value::s, e, c, d, m)
  | (s, e, (LDC n)::c, d, m) ->
    let value = Int n in
    (value::s, e, c, d, m)
  | ((Int v1)::(Int v2)::s, e, ADD::c, d, m) ->
    let value = (Int (v2 + v1)) in
    (value::s, e, c, d, m)
  | ((Int v1)::(Int v2)::s, e, SUB::c, d, m) ->
    let value = (Int (v2 - v1)) in
    (value::s, e, c, d, m)
  | ((Int v1)::(Int v2)::s, e, MUL::c, d, m) ->
    let value = (Int (v2 * v1)) in
    (value::s, e, c, d, m)
  | (s, e, (LDF c')::c, d, m) ->
    let address = next m in
    ((Address address)::s, e, c, d, add address (c', e) m)
  | (v::(Address a)::s, e, AP::c, d, m) ->
    let (c', e') = get a m in
    ([], v::e', c', (s, e, c)::d, m)
  | (v::_, _, RTN::_, (s',e',c')::d, m) ->
    (v::s', e', c', d, m)
  | ((Int 0)::s, e, SEL (c1, _)::c, d, m) ->
    (s, e, c1, ([], [], c)::d, m)
  | ((Int _)::s, e, SEL (_, c2)::c, d, m) ->
    (s, e, c2, ([], [], c)::d, m)
  | (s, e, JOIN::_, (_, _, c')::d, m) ->
    (s, e, c', d, m)
  | (s, e, (LDFR c')::c, d, m) ->
    let address = next m in
    let new_m = add address (c', (Address address)::e) m in
    ((Address address)::s, e, c, d, new_m)
  | _ -> failwith "error"

let rec execute_t c =
  match execute c with
  | (v::[], _, [], _, _) -> v
  | c' -> execute_t c'
