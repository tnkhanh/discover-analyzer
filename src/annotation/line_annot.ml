(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module BG = Bug

(*******************************************************************
 ** Data structures representing bug annotations
 *******************************************************************)

type line_annot =
  int * string * (BG.bug_type list)

type line_annots = line_annot list

type program = line_annots

(*******************************************************************
 ** Printing functions
 *******************************************************************)

let get_line (ann : line_annot) : int =
  match ann with
  | (l, _, _) -> l
;;

let get_ann_type (ann : line_annot) : string =
  match ann with
  | (_, a, _) -> a
;;

let get_bug_types (ann : line_annot) : BG.bug_type list =
  match ann with
  | (_, _, bug_types) -> bug_types
;;

let pr_line_annot (ann : line_annot) : string =
  match ann with
  | (line, atype, bugs) ->
    "Line annotation: Line: " ^ pr_int line ^ "; Type: " ^ atype ^ "; "
    ^ pr_list_plain ~f:BG.pr_bug_type bugs
;;
