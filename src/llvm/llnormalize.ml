(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
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
  let process_instr i =
    let vi = llvalue_of_instr i in
    match LL.instr_opcode vi with
    | LO.Br _ | LO.IndirectBr | LO.Resume | LO.Store | LO.Ret | LO.Unreachable
      -> ()
    | LO.Call when is_type_void (type_of_instr i) -> ()
    | LO.Invoke when is_type_void (type_of_instr i) -> ()
    | _ ->
      if (not !report_source_code_name) || is_llvalue_empty_name vi
      then (
        let instr_name = "v" ^ compute_index index_value in
        LL.set_value_name instr_name vi) in
  let process_param p =
    let vp = llvalue_of_param p in
    let param_name = "arg" ^ compute_index index_value in
    LL.set_value_name param_name vp in
  let process_global g =
    let vg = llvalue_of_global g in
    if (not !report_source_code_name) || is_llvalue_empty_name vg
    then (
      let global_name = "g" ^ compute_index index_value in
      LL.set_value_name global_name vg) in
  let process_block blk =
    let blk_name = "bb" ^ compute_index index_blk in
    let _ = LL.set_value_name blk_name (LL.value_of_block blk) in
    None in
  let process_func f =
    (* reset index for each function *)
    let _ = index_blk := -1 in
    None in
  visit_module ~fglobal:(Some process_global) ~ffunc:(Some process_func)
    ~fparam:(Some process_param) ~fblock:(Some process_block)
    ~finstr:(Some process_instr) modul
;;

(*******************************************************************
 ** eliminate instruction
 *******************************************************************)

(** remove calls to intrinsics: llvm.lifetime.start, llvm.lifetime.end *)
let elim_instr_intrinsic_lifetime (modul : LL.llmodule) : unit =
  let finstr =
    Some
      (fun acc instr ->
        match instr_opcode instr with
        | LO.Call ->
          let callee = callee_of_instr_call instr in
          let fname = func_name callee in
          if String.is_prefix fname ~prefix:"llvm.lifetime"
          then acc @ [ instr ]
          else acc
        | _ -> acc) in
  let instr_calls = visit_fold_module ~finstr [] modul in
  let callees =
    List.fold_left
      ~f:(fun acc instr ->
        let callee = callee_of_instr_call instr in
        if List.mem ~equal:equal_func acc callee then acc else acc @ [ callee ])
      ~init:[] instr_calls in
  let _ = List.iter ~f:delete_instruction instr_calls in
  List.iter ~f:delete_function callees
;;

let elim_unused_instructions (modul : LL.llmodule) : unit =
  let rec elim_each_func (func : func) =
    (* first collect unused instructions *)
    let finstr =
      Some
        (fun acc instr ->
          match instr_opcode instr with
          | LO.Load ->
            let vinstr = llvalue_of_instr instr in
            (match LL.use_begin vinstr with
            | None -> acc @ [ instr ]
            | Some _ -> acc)
          | _ -> acc) in
    let unused_instrs = visit_fold_func ~finstr [] func in
    (* then remove them *)
    if unused_instrs != []
    then (
      let _ = List.iter ~f:delete_instruction unused_instrs in
      elim_each_func func)
    else () in
  iter_functions ~f:elim_each_func modul
;;

(** eliminate instruction Load that loads from a register a constant that was
    stored previously in the same basic block *)
let elim_instr_load_of_const (modul : LL.llmodule) : unit =
  let find_instr_load_and_replacer (func : func) : (instr * value) list =
    let loads_replacers = ref [] in
    let tbl_stored_values = Hashtbl.create (module InstrKey) in
    let process_instr instr =
      match instr_opcode instr with
      | LO.Store ->
        let src = src_of_instr_store instr in
        let dst = dst_of_instr_store instr in
        if is_llvalue_instr dst && is_llvalue_integer_constant src
        then Hashtbl.set ~key:(mk_instr dst) ~data:src tbl_stored_values
      | LO.Load ->
        let src = src_of_instr_load instr in
        if is_llvalue_instr src
        then (
          let instr_src = mk_instr src in
          match Hashtbl.find tbl_stored_values instr_src with
          | Some v ->
            if is_instr_same_block instr instr_src
            then loads_replacers := !loads_replacers @ [ instr, v ]
          | None -> ())
        else ()
      | LO.Call | LO.CallBr | LO.Invoke ->
        (* If a register is passed as an argument of a function call,
           then clear its stored constant, if any *)
        let args = args_of_instr_func_app instr in
        List.iter
          ~f:(fun arg ->
            if is_llvalue_instr arg
            then Hashtbl.remove tbl_stored_values (mk_instr arg)
            else ())
          args
      | _ -> () in
    let _ = visit_func ~finstr:(Some process_instr) func in
    !loads_replacers in
  let elim_instr_load (func : func) : unit =
    (* let _ = debugpc "elim_instr_load: " func_name func in *)
    let continue = ref true in
    while !continue do
      let load_replacers = find_instr_load_and_replacer func in
      if List.is_empty load_replacers
      then continue := false
      else
        List.iter
          ~f:(fun (instr_load, replacer) ->
            let vload = llvalue_of_instr instr_load in
            let process_instr instr =
              for i = 0 to num_operands instr do
                if equal_value vload (operand instr i)
                then set_operand instr i replacer
              done in
            let _ = visit_func ~finstr:(Some process_instr) func in
            let _ = delete_instruction instr_load in
            continue := true)
          load_replacers
    done in
  iter_functions ~f:elim_instr_load modul
;;

(** Eliminate instruction SExt of integers, e.g. sext i32 0 to i64.
    Such instructions SExt may be introduced from other simplifications *)
let elim_instr_sext_integer (modul : LL.llmodule) : unit =
  let find_instr_sext_and_replacer (func : func) : (instr * value) list =
    let sext_replacers = ref [] in
    let process_instr instr =
      match instr_opcode instr with
      | LO.SExt ->
        let src = src_of_instr_sext instr in
        if is_llvalue_integer_constant src
        then (
          let dst_typ = LL.type_of (llvalue_of_instr instr) in
          match LL.int64_of_const src with
          | Some i ->
            let replacer = LL.const_of_int64 dst_typ i true in
            sext_replacers := !sext_replacers @ [ instr, replacer ]
          | None -> ())
        else ()
      | _ -> () in
    let _ = visit_func ~finstr:(Some process_instr) func in
    !sext_replacers in
  let elim_instr_sext (func : func) : unit =
    let continue = ref true in
    while !continue do
      let sext_replacers = find_instr_sext_and_replacer func in
      if List.is_empty sext_replacers
      then continue := false
      else
        List.iter
          ~f:(fun (instr_sext, replacer) ->
            let vsext = llvalue_of_instr instr_sext in
            let process_instr instr =
              for i = 0 to num_operands instr do
                if equal_value vsext (operand instr i)
                then set_operand instr i replacer
              done in
            let _ = visit_func ~finstr:(Some process_instr) func in
            let _ = delete_instruction instr_sext in
            continue := true)
          sext_replacers
    done in
  iter_functions ~f:elim_instr_sext modul
;;

(*******************************************************************
 ** finally normalize all
 *******************************************************************)

let normalize_module (filename : string) (modul : LL.llmodule) : unit =
  let _ = debug ("Normalize module: " ^ filename) in
  let _ = elim_instr_intrinsic_lifetime modul in
  let _ = elim_unused_instructions modul in
  (* let _ = elim_unused_functions modul in *)
  let _ = elim_instr_load_of_const modul in
  let _ = elim_instr_sext_integer modul in
  ()
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
            ^ (pr_instr instr ^ "\n")
            ^ "Only Ret, Br, IndirectBr, Switch, Call, Invoke, Resume, "
            ^ "or Unreachable is allowed!")) in
    Some () in
  visit_module ~fblock:(Some fblock) modul
;;
