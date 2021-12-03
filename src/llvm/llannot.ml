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

type bug_annot =
  | Bug_start of (int * int) * BG.bug_type list
  | Bug_end of (int * int)
  | Safe_start of (int * int) * BG.bug_type list
  | Safe_end of (int * int)
  | Skip

type bug_annots = bug_annot list

(*******************************************************************
 ** Printing functions
 *******************************************************************)

let pos_of_annot (ann : bug_annot) =
  match ann with
  | Bug_start (p, _) -> p
  | Bug_end p -> p
  | Safe_start (p, _) -> p
  | Safe_end p -> p
  | Skip -> -1, -1
;;

let pr_pos_ann ann =
  let line, col = pos_of_annot ann in
  pr_int line ^ " " ^ pr_int col
;;

type program = bug_annot list

let pr_bug_annot (annot: bug_annot) : string =
  match annot with
  | Bug_start ((x, y), bl) ->
    "Bug_start " ^ pr_int x ^ " " ^ pr_int y ^ " " ^
    (pr_list_plain ~f:BG.pr_bug_type bl)
  | Bug_end (x, y) -> "Bug_end " ^ pr_int x ^ " " ^ pr_int y
  | Safe_start ((x, y), bl) ->
    "Safe_start " ^ pr_int x ^ " " ^ pr_int y ^ " " ^
    (pr_list_plain ~f:BG.pr_bug_type bl)
  | Safe_end (x, y) -> "Safe_end " ^ pr_int x ^ " " ^ pr_int y
  | Skip -> "Skip_"
;;
