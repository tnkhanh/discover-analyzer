(********************************************************************
 * Author: Ta Quang Trung
 * Date: 2020
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore
open Llir

module LL = Llvm
module LO = Llvm.Opcode
module LV = Llvm.ValueKind

(*******************************************************************
 ** eliminate instruction
 *******************************************************************)

(** remove calls to intrinsics: llvm.lifetime.start, llvm.lifetime.end *)
let elim_instr_intrinsic_lifetime (modul: LL.llmodule) : unit =
  let finstr = Some (fun acc instr ->
    match instr_opcode instr with
    | LO.Call ->
      let callee = callee_of_instr_call instr in
      let fname = func_name callee in
      if (String.is_prefix fname ~prefix:"llvm.lifetime") then
        acc @ [instr]
      else acc
    | _ -> acc) in
  let instr_calls = deep_fold_module ~finstr [] modul in
  let callees = List.fold_left ~f:(fun acc instr ->
    let callee = callee_of_instr_call instr in
    if List.mem ~equal:equal_func acc callee then acc
    else acc @ [callee]) ~init:[] instr_calls in
  let _ = List.iter ~f:delete_instruction instr_calls in
  List.iter ~f:delete_function callees

let elim_unused_instructions (modul: LL.llmodule) : unit =
  let rec elim_each_func (func: func) =
    (* first collect unused instructions *)
    let finstr = Some (fun acc instr ->
      match instr_opcode instr with
      | LO.Load ->
        let vinstr = llvalue_of_instr instr in
        (match LL.use_begin vinstr with
         | None -> acc @ [instr]
         | Some _ -> acc)
      | _ -> acc) in
    let unused_instrs = deep_fold_func ~finstr [] func in
    (* then remove them *)
    if unused_instrs != [] then
      let _ = List.iter ~f:delete_instruction unused_instrs in
      elim_each_func func
    else () in
  iter_functions ~f:elim_each_func modul

(** eliminate instruction Load that loads from a register a constant that was
    stored previously in the same basic block *)
let elim_instr_load_of_const (modul: LL.llmodule) : unit =
  let find_instr_load_and_replacer (func: func) : (instr * llvalue) list =
    let loads_replacers = ref [] in
    let tbl_stored_values = Hashtbl.create (module Instr) in
    let finstr = Some (fun instr ->
      match instr_opcode instr with
      | LO.Store ->
        let src = src_of_instr_store instr in
        let dst = dst_of_instr_store instr in
        if is_llvalue_instr dst && is_llvalue_integer_constant src then
          Hashtbl.set ~key:(mk_instr dst) ~data:src tbl_stored_values
      | LO.Load ->
        let src = src_of_instr_load instr in
        if is_llvalue_instr src then
          let instr_src = mk_instr src in
          match Hashtbl.find tbl_stored_values instr_src with
          | Some v ->
            if is_instr_same_block instr instr_src then
              loads_replacers := !loads_replacers @ [(instr, v)]
          | None -> ()
        else ()
      | _ -> ()) in
    let _ = deep_iter_func ~finstr func in
    !loads_replacers in
  let elim_instr_load (func: func) : unit =
    (* let _ = hdebugc "elim_instr_load: " func_name func in *)
    let continue = ref true in
    while !continue do
      let load_replacers = find_instr_load_and_replacer func in
      if List.is_empty load_replacers then continue := false
      else
        List.iter ~f:(fun (instr_load, replacer) ->
          let vload = llvalue_of_instr instr_load in
          let finstr = Some (fun instr ->
            for i = 0 to (num_operands instr) do
              if equal_llvalue vload (operand instr i) then
                set_operand instr i replacer
            done;) in
          let _ = deep_iter_func ~finstr func in
          let _ = delete_instruction instr_load in
          continue := true) load_replacers
    done in
  iter_functions ~f:elim_instr_load modul

(** Eliminate instruction SExt of integers, e.g. sext i32 0 to i64.
    Such instructions SExt may be introduced from other simplifications *)
let elim_instr_sext_integer (modul: LL.llmodule) : unit =
  let find_instr_sext_and_replacer (func: func) : (instr * llvalue) list =
    let sext_replacers = ref [] in
    let finstr = Some (fun instr ->
      match instr_opcode instr with
      | LO.SExt ->
        let src = src_of_instr_sext instr in
        if is_llvalue_integer_constant src then
          let dst_typ = LL.type_of (llvalue_of_instr instr) in
          match LL.int64_of_const src with
          | Some i ->
            let replacer = LL.const_of_int64 dst_typ i true in
            sext_replacers := !sext_replacers @ [(instr, replacer)]
          | None -> ()
        else ()
      | _ -> ()) in
    let _ = deep_iter_func ~finstr func in
    !sext_replacers in
  let elim_instr_sext (func: func) : unit =
    let continue = ref true in
    while !continue do
      let sext_replacers = find_instr_sext_and_replacer func in
      if List.is_empty sext_replacers then continue := false
      else
        List.iter ~f:(fun (instr_sext, replacer) ->
          let vsext = llvalue_of_instr instr_sext in
          let finstr = Some (fun instr ->
            for i = 0 to (num_operands instr) do
              if equal_llvalue vsext (operand instr i) then
                set_operand instr i replacer
            done;) in
          let _ = deep_iter_func ~finstr func in
          let _ = delete_instruction instr_sext in
          continue := true) sext_replacers
    done in
  iter_functions ~f:elim_instr_sext modul

(*******************************************************************
 ** finally simplify and normalize all
 *******************************************************************)

let simplify_module (filename: string) (modul: LL.llmodule) : unit =
  let _ = hdebug "Simplifying module: " pr_str filename in
  let _ =
    (* let _ = debug " - Eliminate intrinsic lifetime instructions..." in *)
    elim_instr_intrinsic_lifetime modul in
  let _ =
    (* let _ = debug " - Eliminate unused instructions..." in *)
    elim_unused_instructions modul in
  (* let _ = elim_unused_functions modul in *)
  let _ =
    (* let _ = debug " - Eliminate load_of_const instructions..." in *)
    elim_instr_load_of_const modul in
  let _ =
    (* let _ = debug " - Eliminate sext integer instructions..." in *)
    elim_instr_sext_integer modul in
  (* let _ = debug " - Finish simplifying!" in *)
  ()
