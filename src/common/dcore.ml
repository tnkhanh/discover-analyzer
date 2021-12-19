(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(** Core module of Discover. To be open by all other modules *)

include Core

(*---------------------
 * Extended libraries
 *--------------------*)

include Extcore.Int
include Extcore.Hashtbl
include Extcore.List
include Extcore.Math
include Extcore.String
include Extcore.Sys
include Extcore.Result

(*---------------------
 * Discover's modules
 *--------------------*)

include Global
include Printer
include Debugger
include Report


(*******************************************************************
 ** Override default printing function to throw some warning
 *******************************************************************)

let print_endline (s : string) =
  let _ =
    warning
      ("DO NOT USE print_endline DIRECTLY. "
       ^ "Use printing functions in Printer.ml instead!") in
  Core.print_endline s
;;

let print_string (s : string) =
  let _ =
    warning
      ("DO NOT USE print_string directly. \n"
       ^ "Use printing functions in Printer.ml instead!") in
  Core.print_string s
;;
