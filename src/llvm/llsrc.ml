(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore

module LL = Llvm
module LI = Llir
module SP = Set.Poly

(*******************************************************************
 ** operations with metadata of source code
 *******************************************************************)

let extract_line_column_from_metadata (md: LL.llvalue) : (int * int) =
  try
    let str = LL.string_of_llvalue md in
    let re = Str.regexp ".*line:[ ]*\\([0-9]+\\).*column:[ ]*\\([0-9]+\\).*" in
    if (Str.string_match re str 0) then (
      let line = int_of_string (Str.matched_group 1 str) in
      let column = int_of_string (Str.matched_group 2 str) in
      (line, column - 1))
    else (-1, -1)
  with _ -> (-1, -1)

let extract_filename_from_metadata (md: LL.llvalue) : string =
  try
    let md_location =
      if (LL.num_operands md > 0) then
        LL.operand md 0
      else failwith "no location information" in
    let md_subprogram =
      if (LL.num_operands md_location > 1) then
        LL.operand md_location 1
      else failwith "no subprogram information" in
    let md_filename = LL.operand md_subprogram 0 in
    let rec extract_filename md_file = match LL.get_mdstring md_file with
      | None -> extract_filename ((LL.operand md_file 0))
      | Some s -> s in
    extract_filename md_filename
  with _ -> ""

let extract_name_from_metadata (md: LL.llvalue) : string =
  let str = LL.string_of_llvalue md in
  let re = Str.regexp ".*name:[ ]*\"\\([a-zA-Z0-9_]+\\)\".*" in
  if (Str.string_match re str 0) then
    Str.matched_group 1 str
  else ""

let location_of_llvalue (v: LL.llvalue) : location =
  let llcontext = LL.global_context () in
  let dbg = LL.metadata v (LL.mdkind_id llcontext "dbg") in
  match dbg with
  | None -> raise Not_found
  | Some md ->
    let line, column = extract_line_column_from_metadata md in
    let filename = extract_filename_from_metadata md in
    mk_location_point filename line column

let location_of_instr (instr: LI.instr) : location =
  location_of_llvalue (LI.llvalue_of_instr instr)

let location_of_func (func: LI.func) : location =
  location_of_llvalue (LI.llvalue_of_func func)
