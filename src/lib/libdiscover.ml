(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(*---------------------
 * Extended libraries
 *--------------------*)

include List
include String
include Sys
include Bigint

(*-----------------
 * Boolean result
 *----------------*)

type bresult = bool option

let is_true (b : bresult) : bool =
  match b with
  | Some true -> true
  | _ -> false
;;

let is_false (b : bresult) : bool =
  match b with
  | Some false -> true
  | _ -> false
;;

let pr_bresult (b : bresult) : string =
  match b with
  | Some true -> "True"
  | Some false -> "False"
  | None -> "Unknown"
;;

(*-----------------
 * Big interger
 *----------------*)

type bint = BInt.big_int
