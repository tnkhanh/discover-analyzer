(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(** Core module of Discover. To be open by all other modules *)

include Core

(*---------------------
 * Extended libraries
 *--------------------*)

include Outils.Int
include Outils.Hashtbl
include Outils.List
include Outils.Math
include Outils.String
include Outils.Sys
include Outils.Result
include Outils.Printer
include Outils.Debugger
include Outils.Report
(* module Report = Outils.Report *)
module Process = Outils.Process

(*---------------------
 * Discover's modules
 *--------------------*)

include Global

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
