(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Libdiscover

module LL = Llvm
module LD = Llvm_debuginfo
module LV = Llvm.ValueKind
module LI = Llir
module SP = Set.Poly

(*******************************************************************
 ** operations with metadata of source code
 *******************************************************************)

let extract_name_from_metadata (md : LL.llvalue) : string =
  let str = LL.string_of_llvalue md in
  let re = Str.regexp ".*name:[ ]*\"\\([a-zA-Z0-9_]+\\)\".*" in
  if Str.string_match re str 0 then Str.matched_group 1 str else ""
;;

let get_original_name_of_llvalue (v : LL.llvalue) : string option =
  (* let _ = hprint "get_original_name_of_llvalue: " LI.pr_value v in *)
  match LL.classify_value v with
  | LV.Instruction _ ->
    let func = LI.func_of_instr (LI.mk_instr v) in
    let _ =
      LI.iter_blocks
        ~f:(fun blk ->
          LI.iter_instrs
            ~f:(fun instr ->
              if LI.is_instr_call_invoke instr
              then (
                let callee = LI.callee_of_instr_func_call instr in
                if LI.is_func_llvm_debug_declare callee
                   || LI.is_func_llvm_debug_value callee
                then () (* hprint "Instr: " LI.pr_instr instr *)
                else ()))
            blk)
        func in
    None
  | _ -> None
;;

let position_of_instr (instr : LI.instr) : position option =
  let vinstr = LI.llvalue_of_instr instr in
  let llctx = LL.global_context () in
  let instr_dbg = LL.metadata vinstr (LL.mdkind_id llctx "dbg") in
  match instr_dbg with
  | None -> None
  | Some instr_dbg_info ->
    let instr_md = LL.value_as_metadata instr_dbg_info in
    let line = LD.di_location_get_line ~location:instr_md in
    let column = LD.di_location_get_column ~location:instr_md in
    let scope_md = LD.di_location_get_scope ~location:instr_md in
    (* let _ = hprint "Line: " pr_int line in *)
    (* let _ = hprint "Column: " pr_int column in *)
    let filename =
      match LD.di_scope_get_file ~scope:scope_md with
      | None -> ""
      | Some file_md -> LD.di_file_get_filename ~file:file_md in
    (* let _ = hprint "Filename: " pr_id filename in *)
    Some (mk_position filename line line column column)
;;
