(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(*-----------------
 * Boolean result
 *----------------*)

type bool_result = bool option

let is_bool_result_true (b : bool_result) : bool =
  match b with
  | Some true -> true
  | _ -> false
;;

let is_bool_result_false (b : bool_result) : bool =
  match b with
  | Some false -> true
  | _ -> false
;;

let is_bool_result_unknown (b : bool_result) : bool =
  match b with
  | None -> true
  | Some res -> res
;;

let pr_bool_result (b : bool_result) : string =
  match b with
  | Some true -> "True"
  | Some false -> "False"
  | None -> "Unknown"
;;
