open Value

let lookup (en: env) x =
  match List.find_opt (
    fun (t, _) ->
      if t = x then true else false
    ) en with
| Some found ->
  let (_, value) = found in
  Some value
| None -> None
