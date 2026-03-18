let lookup x l =
  List.find_index (fun e -> e = x) l |> Option.get
