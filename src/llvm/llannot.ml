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

type annot_position =
  { apos_line : int;
    apos_col : int
  }

type bug_annot =
  | Start of annot_position * string * BG.bug_type list
  | End of annot_position * string
  | Line of int * string * BG.bug_type list
  | Skip

type bug_annots = bug_annot list
type program = bug_annots

(*******************************************************************
 ** Printing functions
 *******************************************************************)

let dummy_pos : annot_position = { apos_line = -1; apos_col = -1 }

let get_annot_position (ann : bug_annot) : annot_position =
  match ann with
  | Start (p, _, _) -> p
  | End (p, _) -> p
  | Line (l, _, _) -> { apos_line = l; apos_col = 0 }
  | Skip -> dummy_pos
;;

let get_annot_type (ann : bug_annot) : string =
  match ann with
  | Start (_, t, _) -> t
  | End (_, t) -> t
  | Line (_, t, _) -> t
  | Skip -> ".."
;;

let get_annot_bugs (ann : bug_annot) : BG.bug_type list =
  match ann with
  | Start (_, _, bugs) -> bugs
  | End _ -> []
  | Line (_, _, bugs) -> bugs
  | Skip -> []
;;

let pr_annot_position (pos : annot_position) : string =
  pr_int pos.apos_line ^ ":" ^ pr_int pos.apos_col
;;

let pr_bug_annot (annot : bug_annot) : string =
  match annot with
  | Start (p, atype, bugs) ->
    "Start: " ^ atype ^ ": " ^ pr_annot_position p ^ " "
    ^ pr_list_plain ~f:BG.pr_bug_type bugs
  | End (p, atype) -> "End: " ^ atype ^ ": " ^ pr_annot_position p
  | Line (l, atype, bugs) ->
    "Line: " ^ atype ^ ": " ^ pr_int l ^ " "
    ^ pr_list_plain ~f:BG.pr_bug_type bugs
  | Skip -> "Skip: "
;;

(*******************************************************************
 ** Constructors
 *******************************************************************)

let mk_annot_position (line : int) (col : int) : annot_position =
  { apos_line = line; apos_col = col }
;;
