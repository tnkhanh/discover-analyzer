open Core
open Libdiscover

type bug_group =
  | MemoryLeak
  | NullPointerDeref
  | BufferOverflow
  | IntegerOverflow
  | IntegerUnderflow
  | DivisionByZero
  | NewType of string

type mark =
    | Bug_start of (int * int) * (bug_group list)
    | Bug_end of (int * int)
    | Safe_start of (int * int) * (bug_group list)
    | Safe_end of (int * int)
    | Skip

let pos_of_ann (ann:mark) =
  match ann with
  | Bug_start (p, _) -> p
  | Bug_end p -> p
  | Safe_start (p, _) -> p
  | Safe_end p -> p
  | Skip -> (-1, -1)

let sprint_pos_ann ann =
  let (line, col) = pos_of_ann ann in
  (sprint_int line) ^ " " ^ (sprint_int col)

type program = mark list

let str_of_bug_group b =
  match b with
  | MemoryLeak -> "MemoryLeak"
  | NullPointerDeref -> "NullPointerDeref"
  | BufferOverflow -> "BufferOverflow"
  | IntegerOverflow -> "IntegerOverflow"
  | IntegerUnderflow -> "IntegerUnderflow"
  | DivisionByZero -> "DivisionByZero"
  | NewType t -> "NewType: " ^ t

let str_of_bg_list l =
  List.fold ~init:"" ~f:(fun s b -> s^" . "^(str_of_bug_group b)) l

let str_of_mark m =
  match m with
  | Bug_start ((x, y), bl) -> "Bug_start " ^ (sprint_int x) ^ " " ^ (sprint_int y)
      ^ " " ^ (str_of_bg_list bl)
  | Bug_end (x, y) -> "Bug_end " ^ (sprint_int x) ^ " " ^ (sprint_int y)
  | Safe_start ((x, y), bl) -> "Safe_start " ^ (sprint_int x) ^ " " ^ (sprint_int y)
      ^ " " ^ (str_of_bg_list bl)

  | Safe_end (x, y) -> "Safe_end " ^ (sprint_int x) ^ " " ^ (sprint_int y)
  | Skip -> "Skip_"

(*let list_of_prog (p:prog) =
  List.fold p ~init:[] ~f:(fun acc m -> (str_of_mark m)::acc)

let str_of_prog (p:prog) =
  String.concat ~sep:" " (list_of_prog p) *)
