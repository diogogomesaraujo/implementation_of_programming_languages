open Value

let lookup (en: env) x =
  List.assoc_opt x en
