(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Debugger
open Libdiscover
open Llir
module LL = Llvm
module LO = Llvm.Opcode
module LV = Llvm.ValueKind

(*******************************************************************
 ** renaming all instructions and blocks to SSA form
 *******************************************************************)

let rename_vars_and_params (modul : LL.llmodule) : unit =
  let index_blk = ref (-1) in
  let index_value = ref (-1) in
  (* let index_param = ref (-1) in
   * let index_global = ref (-1) in
   * let index_instr = ref (-1) in *)
  let compute_index index =
    index := !index + 1;
    sprint_int !index in
  let visit_instr i =
    let vi = llvalue_of_instr i in
    match LL.instr_opcode vi with
    | LO.Br _ | LO.IndirectBr | LO.Resume | LO.Store | LO.Ret | LO.Unreachable
      -> ()
    | LO.Call when is_type_void (type_of_instr i) -> ()
    | LO.Invoke when is_type_void (type_of_instr i) -> ()
    | _ ->
      if (not !llvm_orig_source_name) || is_llvalue_empty_name vi
      then (
        let instr_name = "v" ^ compute_index index_value in
        LL.set_value_name instr_name vi) in
  let visit_param p =
    let vp = llvalue_of_param p in
    let param_name = "arg" ^ compute_index index_value in
    LL.set_value_name param_name vp in
  let visit_global g =
    let vg = llvalue_of_global g in
    if (not !llvm_orig_source_name) || is_llvalue_empty_name vg
    then (
      let global_name = "g" ^ compute_index index_value in
      LL.set_value_name global_name vg) in
  let visit_block blk =
    let blk_name = "bb" ^ compute_index index_blk in
    LL.set_value_name blk_name (LL.value_of_block blk);
    None in
  let visit_func f =
    (* reset index for each function *)
    let _ = index_blk := -1 in
    None in
  deep_iter_module
    ~fglobal:(Some visit_global)
    ~ffunc:(Some visit_func)
    ~fparam:(Some visit_param)
    ~fblock:(Some visit_block)
    ~finstr:(Some visit_instr)
    modul
;;

(*******************************************************************
 ** check if bitcode are in the normalized form
 *******************************************************************)

let check_normalization (modul : LL.llmodule) : unit =
  let _ = debug "Checking normalized module..." in
  let fblock blk =
    let _ =
      match last_instr_of_block blk with
      | None -> ()
      | Some instr ->
        (match instr_opcode instr with
        | LO.Ret
        | LO.Br
        | LO.IndirectBr
        | LO.Switch
        | LO.Call
        | LO.Invoke
        | LO.Resume
        | LO.Unreachable -> ()
        | _ ->
          error
            (("The last instruction of block '" ^ block_name blk ^ "' is: ")
            ^ (sprint_instr instr ^ "\n")
            ^ "Only Ret, Br, IndirectBr, Switch, Call, Invoke, Resume, "
            ^ "or Unreachable is allowed!")) in
    None in
  deep_iter_module ~fblock:(Some fblock) modul
;;
