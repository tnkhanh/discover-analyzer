(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core

(*******************************************************************
 * Extending library Result with new functionalities
 *******************************************************************)

module Result = struct
  (*** Exceptions to store results ***)

  exception ResBool of bool
  exception ResInt of int
  exception ResFloat of float
  exception ResString of string

  (*** Functions to raise results ***)

  let return_bool b = raise (ResBool b)
  let return_int i = raise (ResInt i)
  let return_float f = raise (ResFloat f)
  let return_string s = raise (ResString s)

  (*** Original Result module ***)

  include Result
end
