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
module LS = Lldebug
module LV = Llvm.ValueKind
module SP = Set.Poly
module LP = Llloop
module LG = Llcallgraph


let print_pointer_stats (modul: LL.llmodule) : unit =
  let num_struct_vars = ref 0 in
  let num_array_vars = ref 0 in
  let num_pointer_vars = ref 0 in
  let num_instrs = ref 0 in
  let num_blks = ref 0 in
  let num_user_funcs = ref 0 in
  let num_func_calls = ref 0 in
  let update_stats_of_llvalue v =
    let t = LL.type_of v in
    let users = get_users v in
    let _ = if is_type_array t then incr num_array_vars in
    let _ = if is_type_struct t then incr num_struct_vars in
    if is_type_pointer t then
      let elem_typ = LL.element_type t in
      let _ =
        if is_type_struct elem_typ then
          incr num_struct_vars
        else if is_type_array elem_typ then
          incr num_array_vars
        else if List.exists ~f:is_llvalue_instr_gep users then
          incr num_array_vars
        else () in
      incr num_pointer_vars in
  let fglobal = Some (fun global ->
    let t = LL.element_type (type_of_global global) in
    let users = get_users (llvalue_of_global global) in
    let _ = if is_type_array t then incr num_array_vars in
    let _ = if is_type_struct t then incr num_struct_vars in
    let _ = if is_type_pointer t then
        let elem_typ = LL.element_type t in
        let _ =
          if is_type_struct elem_typ then
            incr num_struct_vars
          else if is_type_array elem_typ then
            incr num_array_vars
          else if List.exists ~f:is_llvalue_instr_gep users then
            incr num_array_vars
          else () in
        incr num_pointer_vars in
    ()) in
  let fparam = Some (fun param ->
    let vparam = llvalue_of_param param in
    update_stats_of_llvalue vparam) in
  let finstr = Some (fun instr ->
    let vinstr = llvalue_of_instr instr in
    let _ = update_stats_of_llvalue vinstr in
    let _ = if is_instr_call_invoke instr then incr num_func_calls in
    incr num_instrs) in
  let fblock = Some (fun blk ->
    let _ = incr num_blks in
    None) in
  let ffunc = Some (fun f ->
    let _ = incr num_user_funcs in
    None) in
  let _ = deep_iter_module ~fglobal ~ffunc ~fparam ~fblock ~finstr modul in
  let stats =
    "\nPointer Statistics:\n" ^
    "  #User funcs: " ^ (pr_int !num_user_funcs) ^ "\n" ^
    "  #Blocks: " ^ (pr_int !num_blks) ^ "\n" ^
    "  #Instrs: " ^ (pr_int !num_instrs) ^ "\n" ^
    "  #Func calls: " ^ (pr_int !num_func_calls) ^ "\n" ^
    "  #Pointer Vars: " ^ (pr_int !num_pointer_vars) ^ "\n" ^
    "  #Struct Vars: " ^ (pr_int !num_struct_vars) ^ "\n" ^
    "  #Array Vars: " ^ (pr_int !num_array_vars) ^ "\n" in
  print ~format:false  ~always:true stats
