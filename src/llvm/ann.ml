open Core

type mark = [
    | `Bug_start of string
    | `Bug_end
    | `Safe_start of string
    | `Safe_end
    | `Word of string
]

type prog =  string list

let str_of_mark m =
  match m with
  | `Bug_start bs -> "Bug_start (" ^ bs ^ ")"
  | `Bug_end -> "Bug_end"
  | `Safe_start ss -> "Safe_start (" ^ ss ^ ")"
  | `Safe_end -> "Safe_end"
  | `Word w -> "_"^w^"_"

(*let list_of_prog (p:prog) =
  List.fold p ~init:[] ~f:(fun acc m -> (str_of_mark m)::acc)

let str_of_prog (p:prog) =
  String.concat ~sep:" " (list_of_prog p) *)
