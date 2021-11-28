(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Libdiscover
open Printer
open Llir
module LL = Llvm
module LO = Llvm.Opcode
module LS = Lldebug
module LV = Llvm.ValueKind
module SP = Set.Poly
module LP = Llloop
module LG = Llcallgraph

let print_pointer_stats (modul : LL.llmodule) : unit =
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
    if is_type_pointer t
    then (
      let elem_typ = LL.element_type t in
      let _ =
        if is_type_struct elem_typ
        then incr num_struct_vars
        else if is_type_array elem_typ
        then incr num_array_vars
        else if List.exists ~f:is_llvalue_instr_gep users
        then incr num_array_vars
        else () in
      incr num_pointer_vars) in
  let visit_global g =
    let t = LL.element_type (type_of_global g) in
    let users = get_users (llvalue_of_global g) in
    let _ = if is_type_array t then incr num_array_vars in
    let _ = if is_type_struct t then incr num_struct_vars in
    if is_type_pointer t
    then (
      let elem_typ = LL.element_type t in
      let _ =
        if is_type_struct elem_typ
        then incr num_struct_vars
        else if is_type_array elem_typ
        then incr num_array_vars
        else if List.exists ~f:is_llvalue_instr_gep users
        then incr num_array_vars
        else () in
      incr num_pointer_vars) in
  let visit_param p =
    let vp = llvalue_of_param p in
    update_stats_of_llvalue vp in
  let visit_instr i =
    let vi = llvalue_of_instr i in
    let _ = update_stats_of_llvalue vi in
    let _ = if is_instr_call_invoke i then incr num_func_calls in
    incr num_instrs in
  let visit_block blk =
    let _ = incr num_blks in
    None in
  let visit_func f =
    let _ = incr num_user_funcs in
    None in
  let _ =
    deep_iter_module ~fglobal:(Some visit_global) ~ffunc:(Some visit_func)
      ~fparam:(Some visit_param) ~fblock:(Some visit_block)
      ~finstr:(Some visit_instr) modul in
  let stats =
    "\nPointer Statistics:\n"
    ^ sprintf "  #User funcs: %d\n" !num_user_funcs
    ^ sprintf "  #Blocks: %d\n" !num_blks
    ^ sprintf "  #Instrs: %d\n" !num_instrs
    ^ sprintf "  #Func calls: %d\n" !num_func_calls
    ^ sprintf "  #Pointer Vars: %d\n" !num_pointer_vars
    ^ sprintf "  #Struct Vars: %d\n" !num_struct_vars
    ^ sprintf "  #Array Vars: %d\n" !num_array_vars in
  print ~format:false ~always:true stats
;;
