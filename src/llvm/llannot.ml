(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module BG = Bug

(*******************************************************************
 ** Data structures representing bug annotations
 *******************************************************************)

type annot_type =
  | Bug
  | Safe

type annot_position =
  { apos_line : int;
    apos_col : int
  }

type bug_annot =
  | Bug_start of annot_position * BG.bug_type list
  | Bug_end of annot_position
  | Safe_start of annot_position * BG.bug_type list
  | Safe_end of annot_position
  | Skip

type bug_annots = bug_annot list

(*******************************************************************
 ** Printing functions
 *******************************************************************)

let dummy_pos : annot_position = { apos_line = -1; apos_col = -1 }

let get_annot_position (ann : bug_annot) : annot_position =
  match ann with
  | Bug_start (p, _) -> p
  | Bug_end p -> p
  | Safe_start (p, _) -> p
  | Safe_end p -> p
  | Skip -> dummy_pos
;;

let pr_annot_position (pos : annot_position) : string =
  pr_int pos.apos_line ^ ":" ^ pr_int pos.apos_col
;;

type program = bug_annot list

let pr_bug_annot (annot : bug_annot) : string =
  match annot with
  | Bug_start (p, bl) ->
    "Bug_start " ^ pr_annot_position p ^ " "
    ^ pr_list_plain ~f:BG.pr_bug_type bl
  | Bug_end p -> "Bug_end " ^ pr_annot_position p
  | Safe_start (p, bl) ->
    "Safe_start " ^ pr_annot_position p ^ " "
    ^ pr_list_plain ~f:BG.pr_bug_type bl
  | Safe_end p -> "Safe_end " ^ pr_annot_position p
  | Skip -> "Skip_"
;;

(*******************************************************************
 ** Constructors
 *******************************************************************)

let mk_annot_position (line : int) (col : int) : annot_position =
  { apos_line = line; apos_col = col }
;;
