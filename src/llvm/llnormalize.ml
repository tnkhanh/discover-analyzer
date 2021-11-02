(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Lib
open Sprinter
open Printer
open Debugger
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
    pr_int !index in
  let finstr =
    Some
      (fun instr ->
        let vinstr = llvalue_of_instr instr in
        match LL.instr_opcode vinstr with
        | LO.Br _
        | LO.IndirectBr
        | LO.Resume
        | LO.Store
        | LO.Ret
        | LO.Unreachable -> ()
        | LO.Call when is_type_void (type_of_instr instr) -> ()
        | LO.Invoke when is_type_void (type_of_instr instr) -> ()
        | _ ->
          if (not !llvm_orig_source_name) || is_llvalue_empty_name vinstr
          then (
            let instr_name = "v" ^ compute_index index_value in
            LL.set_value_name instr_name vinstr)) in
  let fparam =
    Some
      (fun param ->
        let vparam = llvalue_of_param param in
        let param_name = "arg" ^ compute_index index_value in
        LL.set_value_name param_name vparam) in
  let fglobal =
    Some
      (fun global ->
        let vglobal = llvalue_of_global global in
        if (not !llvm_orig_source_name) || is_llvalue_empty_name vglobal
        then (
          let global_name = "g" ^ compute_index index_value in
          LL.set_value_name global_name vglobal)) in
  let fblock =
    Some
      (fun blk ->
        let blk_name = "bb" ^ compute_index index_blk in
        LL.set_value_name blk_name (LL.value_of_block blk);
        None) in
  let ffunc =
    Some
      (fun func ->
        (* reset index for each function *)
        (* index_blk := -1; index_instr := -1; index_param := -1; *)
        index_blk := -1;
        None) in
  deep_iter_module ~fglobal ~ffunc ~fparam ~fblock ~finstr modul
;;

(*******************************************************************
 ** check if bitcode are in the normalized form
 *******************************************************************)

let check_normalization (modul : LL.llmodule) : unit =
  let _ = debug "Checking normalized module..." in
  let fblock =
    Some
      (fun blk ->
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
                ("The last instruction of block '"
                ^ block_name blk
                ^ "' is: "
                ^ pr_instr instr
                ^ "\n"
                ^ "Only Ret, Br, IndirectBr, Switch, Call, Invoke, Resume, "
                ^ "or Unreachable is allowed!")) in
        None) in
  deep_iter_module ~fblock modul
;;
