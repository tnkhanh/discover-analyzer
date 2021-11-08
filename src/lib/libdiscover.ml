(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(*---------------------
 * Extended libraries
 *--------------------*)

include Libbigint
include Libhashtbl
include Liblist
include Libmath
include Libstring
include Libsys
include Sprinter

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

let sprint_bresult (b : bresult) : string =
  match b with
  | Some true -> "True"
  | Some false -> "False"
  | None -> "Unknown"
;;

(*-----------------
 * Big interger
 *----------------*)

type bint = BInt.big_int


(*-----------------
 * Exceptions
 *----------------*)

exception EInt of int
exception EBool of bool
exception EError of (string * string)
exception EString of string
exception ESkip
exception EDone

let raise_bool b = raise (EBool b)
let raise_int i = raise (EInt i)
