open Execute
open Types

let run i =
  execute_t ([], [], i, [], StoreMap.empty)
