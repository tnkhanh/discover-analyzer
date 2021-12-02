(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Dfdata
open Bug
module LL = Llvm
module LI = Llir

(*******************************************************************
 ** Integer bugs
 *******************************************************************)

(*-------------------
 * Integer overflow
 *------------------*)

let check_bug_integer_overflow (pdata : program_data) (pbug : potential_bug)
    : bug option
  =
  let open Option.Let_syntax in
  match pbug.pbug_type with
  | IntegerOverflow iof ->
    if !bug_integer_all || !bug_integer_overflow
    then (
      let _ = hdebug "Checking Potential Bug: " pr_potential_bug pbug in
      let func = LI.func_of_instr iof.iof_instr in
      let%bind penv_rng = pdata.pdata_env_range in
      let%bind fenvs_rng = Hashtbl.find penv_rng.penv_func_envs func in
      List.fold_left
        ~f:(fun acc fenv ->
          if acc != None
          then acc
          else (
            let%bind data = RG.get_instr_output fenv iof.iof_instr in
            let itv = RG.get_interval (LI.expr_of_llvalue iof.iof_expr) data in
            match itv with
            | Bottom -> None
            | Range r ->
              let ub =
                EInt.compute_upper_bound_two_complement iof.iof_bitwidth in
              if RG.ID.compare_bound r.range_ub (EInt ub) > 0
              then (
                let reason =
                  ("Variable " ^ LI.pr_value iof.iof_expr ^ " can take only ")
                  ^ ("the maximum value of: " ^ EInt.pr_eint ub ^ ",\n")
                  ^ "but is assigned with: " ^ RG.pr_bound r.range_ub ^ ".\n" in
                return (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
              else None))
        ~init:None fenvs_rng)
    else None
  | _ -> None
;;

let find_bug_integer_overflow (pdata : program_data) =
  if !bug_all || !bug_integer_all || !bug_integer_overflow
  then
    let _ = debug "Finding All Integer Overflow Bugs..." in
    pdata.pdata_potential_bugs
    |> List.map ~f:(check_bug_integer_overflow pdata)
    |> List.filter_opt
  else []
;;

(*--------------------
 * Integer underflow
 *-------------------*)

let check_bug_integer_underflow (pdata : program_data) (pbug : potential_bug)
    : bug option
  =
  let open Option.Let_syntax in
  match pbug.pbug_type with
  | IntegerUnderflow iuf ->
    if !bug_integer_all || !bug_integer_overflow
    then (
      let func = LI.func_of_instr iuf.iuf_instr in
      let%bind penv_rng = pdata.pdata_env_range in
      let%bind fenvs_rng = Hashtbl.find penv_rng.penv_func_envs func in
      List.fold_left
        ~f:(fun acc fenv ->
          if acc != None
          then acc
          else (
            let%bind data = RG.get_instr_output fenv iuf.iuf_instr in
            let itv = RG.get_interval (LI.expr_of_llvalue iuf.iuf_expr) data in
            match itv with
            | Bottom -> None
            | Range r ->
              let lb =
                EInt.compute_lower_bound_two_complement iuf.iuf_bitwidth in
              if RG.ID.compare_bound r.range_lb (EInt lb) < 0
              then (
                let reason =
                  ("Variable " ^ LI.pr_value iuf.iuf_expr ^ " can take only ")
                  ^ ("the minimum value of: " ^ EInt.pr_eint lb ^ ",\n")
                  ^ "but is assigned with: " ^ RG.pr_bound r.range_lb ^ ".\n" in
                return (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
              else None))
        ~init:None fenvs_rng)
    else None
  | _ -> None
;;

let find_bug_integer_underflow (pdata : program_data) =
  if !bug_all || !bug_integer_all || !bug_integer_underflow
  then
    let _ = debug "Finding All Integer Underflow Bugs..." in
    pdata.pdata_potential_bugs
    |> List.map ~f:(check_bug_integer_underflow pdata)
    |> List.filter_opt
  else []
;;

(*******************************************************************
 ** Memory bugs
 *******************************************************************)

(*-------------------
 * Buffer overflown
 *------------------*)

let check_bug_buffer_overflow (pdata : program_data) (pbug : potential_bug)
    : bug option
  =
  let open Option.Let_syntax in
  match pbug.pbug_type with
  | BufferOverflow bof ->
    let ptr = bof.bof_pointer in
    let func = LI.func_of_instr bof.bof_instr in
    let%bind penv_rng = pdata.pdata_env_range in
    let%bind fenvs_rng = Hashtbl.find penv_rng.penv_func_envs func in
    List.fold_left
      ~f:(fun acc fenv_rng ->
        if acc != None
        then acc
        else (
          let%bind data_rng = RG.get_instr_output fenv_rng bof.bof_instr in
          let itv =
            RG.get_interval (LI.expr_of_llvalue bof.bof_elem_index) data_rng
          in
          match bof.bof_buff_size with
          | NumElem (n, t) ->
            if RG.ID.compare_interval_ub_int itv n >= 0
            then (
              let reason =
                ("Buffer at pointer " ^ LI.pr_value ptr)
                ^ (" contains " ^ pr_int64 n ^ " elements;\n")
                ^ "accessing index is " ^ RG.pr_interval_concise itv in
              return (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
            else None
          | MemSizeOf v ->
            let vinstr = LI.mk_instr v in
            let%bind penv_msz = pdata.pdata_env_memsize in
            let%bind fenvs_msz = Hashtbl.find penv_msz.penv_func_envs func in
            List.fold_left
              ~f:(fun acc fenv_msz ->
                if acc != None
                then acc
                else (
                  let%bind data_msz = MS.get_instr_output fenv_msz vinstr in
                  let sz = MS.get_size v data_msz in
                  let elem_typ = LL.element_type (LL.type_of v) in
                  let elem_size = LI.memsize_of_type elem_typ in
                  let max_num_elem = Int64.( / ) sz.size_max elem_size in
                  let min_num_elem = Int64.( / ) sz.size_min elem_size in
                  if RG.ID.compare_interval_ub_int itv max_num_elem >= 0
                  then (
                    let reason =
                      ("Buffer at pointer " ^ LI.pr_value ptr ^ " contains ")
                      ^ ("at most " ^ pr_int64 max_num_elem ^ " elements;\n")
                      ^ "accessing index is " ^ RG.pr_interval_concise itv
                    in
                    return (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
                  else if RG.ID.compare_interval_ub_int itv min_num_elem >= 0
                  then (
                    let reason =
                      ("Buffer at pointer " ^ LI.pr_value ptr ^ " may contain ")
                      ^ ("only " ^ pr_int64 min_num_elem ^ " elements;\n")
                      ^ "accessing index is " ^ RG.pr_interval_concise itv
                    in
                    return (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
                  else None))
              ~init:None fenvs_msz))
      ~init:None fenvs_rng
  | _ -> None
;;

let find_bug_buffer_overflow (pdata : program_data) : bug list =
  if !bug_all || !bug_memory_all || !bug_buffer_overflow
  then
    let _ = debug "Finding All Buffer Overflow Bugs..." in
    pdata.pdata_potential_bugs
    |> List.map ~f:(check_bug_buffer_overflow pdata)
    |> List.filter_opt
  else []
;;

(*--------------------
 * Check memory leak
 *-------------------*)

let check_bug_memory_leak (pdata : program_data) (pbug : potential_bug)
    : bug option
  =
  match pbug.pbug_type with
  | MemoryLeak mlk ->
    let _ = print "check_bug_memory_leak: TO IMPLEMENT CHECK MEMORY LEAK" in
    None
  | _ -> None
;;

let find_bug_memory_leak (pdata : program_data) : bug list =
  if !bug_all || !bug_memory_all || !bug_memory_leak
  then
    let _ = debug "Finding All Memory Leak Bugs..." in
    pdata.pdata_potential_bugs
    |> List.map ~f:(check_bug_memory_leak pdata)
    |> List.filter_opt
  else []
;;

(** TODO: need a mechanism to schedule analyses based on bugs:
    - Indetify the type of bugs will be checked
    - Determine which analyeses need to be performed. *)

let find_bugs (pdata : program_data) : unit =
  let _ = println "Checking Bugs..." in
  let _ =
    hddebug ~header:true "Annotated potential bugs: " pr_potential_bugs
      pdata.pdata_potential_bugs in
  let bugs =
    find_bug_memory_leak pdata
    @ find_bug_buffer_overflow pdata
    @ find_bug_integer_overflow pdata
    @ find_bug_integer_underflow pdata in
  let _ = List.iter ~f:(fun bug -> print ~marker:false (pr_bug bug)) bugs in
  let _ = num_of_bugs := List.length bugs in
  report_bug_stats bugs
;;
