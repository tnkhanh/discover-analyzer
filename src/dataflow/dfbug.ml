(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Dfdata
open Bug
module LL = Llvm
module LI = Llir
module II = Int_interval

(* analysis *)
(* module PT = Pointer.Analysis *)
module MS = Memsize.Analysis

(* module MT = Memtype.Analysis *)
module RG = Range.Analysis
(* module UD = Undef.Analysis *)

(*******************************************************************
 ** Integer bugs
 *******************************************************************)

module IntegerBug = struct
  (*-------------------
   * Integer overflow
   *------------------*)

  let check_bug_integer_overflow (dfa : dfa_data) (pbug : potential_bug)
      : bug option
    =
    let open Option.Let_syntax in
    match pbug.pbug_type with
    | IntegerOverflow (Some iof) ->
      if !bug_integer_all || !bug_integer_overflow
      then (
        let _ = debugp "Checking Potential Bug: " pr_potential_bug pbug in
        let func = LI.func_of_instr iof.iof_instr in
        let%bind penv_rng = dfa.dfa_env_range in
        let%bind fenvs_rng = Hashtbl.find penv_rng.penv_func_envs func in
        List.fold_left
          ~f:(fun acc fenv ->
            if acc != None
            then acc
            else (
              let%bind data = RG.get_instr_output fenv iof.iof_instr in
              let itv = RG.get_interval (LI.expr_of_value iof.iof_expr) data in
              match itv with
              | Bottom -> None
              | Range r ->
                let ub =
                  EInt.compute_upper_bound_two_complement iof.iof_bitwidth
                in
                if II.compare_bound r.range_ub (EInt ub) > 0
                then (
                  let reason =
                    ("Variable " ^ LI.pr_value iof.iof_expr ^ " can take only ")
                    ^ ("the maximum value of: " ^ EInt.pr_eint ub ^ ",\n")
                    ^ "but is assigned with: " ^ RG.pr_bound r.range_ub ^ ".\n"
                  in
                  Some (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
                else None))
          ~init:None fenvs_rng)
      else None
    | _ -> None
  ;;

  let find_bug_integer_overflow (dfa : dfa_data) =
    if !bug_all || !bug_integer_all || !bug_integer_overflow
    then (
      let _ = debug "Finding All Integer Overflow Bugs..." in
      dfa.dfa_potential_bugs
      |> List.map ~f:(check_bug_integer_overflow dfa)
      |> List.filter_opt)
    else []
  ;;

  (*--------------------
   * Integer underflow
   *-------------------*)

  let check_bug_integer_underflow (dfa : dfa_data) (pbug : potential_bug)
      : bug option
    =
    let open Option.Let_syntax in
    match pbug.pbug_type with
    | IntegerUnderflow (Some iuf) ->
      if !bug_integer_all || !bug_integer_overflow
      then (
        let func = LI.func_of_instr iuf.iuf_instr in
        let%bind penv_rng = dfa.dfa_env_range in
        let%bind fenvs_rng = Hashtbl.find penv_rng.penv_func_envs func in
        List.fold_left
          ~f:(fun acc fenv ->
            if acc != None
            then acc
            else (
              let%bind data = RG.get_instr_output fenv iuf.iuf_instr in
              let itv = RG.get_interval (LI.expr_of_value iuf.iuf_expr) data in
              match itv with
              | Bottom -> None
              | Range r ->
                let lb =
                  EInt.compute_lower_bound_two_complement iuf.iuf_bitwidth
                in
                if II.compare_bound r.range_ub (EInt lb) < 0
                then (
                  let reason =
                    ("Variable " ^ LI.pr_value iuf.iuf_expr ^ " can take only ")
                    ^ ("the minimum value of: " ^ EInt.pr_eint lb ^ ",\n")
                    ^ "but is assigned with: " ^ RG.pr_bound r.range_ub ^ ".\n"
                  in
                  Some (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
                else None))
          ~init:None fenvs_rng)
      else None
    | _ -> None
  ;;

  let find_bug_integer_underflow (dfa : dfa_data) : bugs =
    if !bug_all || !bug_integer_all || !bug_integer_underflow
    then (
      let _ = debug "Finding All Integer Underflow Bugs..." in
      dfa.dfa_potential_bugs
      |> List.map ~f:(check_bug_integer_underflow dfa)
      |> List.filter_opt)
    else []
  ;;

  (*--------------------
   * Division by Zero
   *-------------------*)

  let check_bug_division_by_zero (dfa : dfa_data) (pbug : potential_bug)
      : bug option
    =
    let open Option.Let_syntax in
    match pbug.pbug_type with
    | DivisionByZero None ->
      if !bug_all || !bug_integer_all || !bug_divizion_by_zero
      then (
        let _ = debugp "Checking Potential Bug: " pr_potential_bug pbug in
        let func = LI.func_of_instr pbug.pbug_instr in
        let%bind penv_rng = dfa.dfa_env_range in
        let%bind fenvs_rng = Hashtbl.find penv_rng.penv_func_envs func in
        List.fold_left
          ~f:(fun acc fenv ->
            if acc != None
            then acc
            else (
              let%bind data = RG.get_instr_output fenv pbug.pbug_instr in
              let divisor = LI.operand pbug.pbug_instr 1 in
              let itv = RG.get_interval (LI.expr_of_value divisor) data in
              match itv with
              | Bottom -> None
              | Range r ->
                if II.compare_bound r.range_ub (Int64 Int64.zero) <= 0
                   && II.compare_bound r.range_ub (Int64 Int64.zero) >= 0
                then (
                  let reason =
                    ("Divisor " ^ LI.pr_value divisor
                   ^ " can take values from ")
                    ^ (RG.pr_bound r.range_ub ^ " to " ^ RG.pr_bound r.range_ub)
                    ^ " and can be zero.\n" in
                  return (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
                else None))
          ~init:None fenvs_rng)
      else None
    | _ -> None
  ;;

  let find_bug_division_by_zero (dfa : dfa_data) : bugs =
    if !bug_all || !bug_integer_all || !bug_divizion_by_zero
    then (
      let _ = debug "Finding All Division by Zero Bugs..." in
      dfa.dfa_potential_bugs
      |> List.map ~f:(check_bug_division_by_zero dfa)
      |> List.filter_opt)
    else []
  ;;
end

include IntegerBug

(*******************************************************************
 ** Memory bugs
 *******************************************************************)

module MemoryBug = struct
  (*-------------------
   * Buffer overflow
   *------------------*)

  let update_buffer_overflow_memory_type
      (dfa : dfa_data)
      (bof : buffer_overflow)
      : unit
    =
    let ptr, ins = bof.bof_pointer, bof.bof_instr in
    if is_stack_based_pointer dfa ins ptr
    then bof.bof_stack_based <- Some true
    else if is_heap_based_pointer dfa ins ptr
    then bof.bof_stack_based <- Some false
    else bof.bof_stack_based <- None
  ;;

  let check_bug_buffer_overflow (dfa : dfa_data) (pbug : potential_bug)
      : bug option
    =
    let open Option.Let_syntax in
    let prog = dfa.dfa_program in
    let data_layout = LI.get_data_layout prog in
    match pbug.pbug_type with
    | BufferOverflow (Some bof) ->
      let ins = bof.bof_instr in
      let func = LI.func_of_instr ins in
      let ptr = bof.bof_pointer in
      let ptr_name = LI.pr_value_source_or_llvm_name ptr in
      let _ = update_buffer_overflow_memory_type dfa bof in
      let%bind penv_rng = dfa.dfa_env_range in
      let%bind fenvs_rng = Hashtbl.find penv_rng.penv_func_envs func in
      List.fold_left
        ~f:(fun acc fenv_rng ->
          if acc != None
          then acc
          else (
            let%bind data_rng = RG.get_instr_output fenv_rng ins in
            let index_itv =
              RG.get_interval (LI.expr_of_value bof.bof_elem_index) data_rng
            in
            let index_itv_str = RG.pr_interval_concise index_itv in
            if LI.is_pointer_to_array ptr
            then (
              let%bind num_elem = LI.compute_array_length ptr in
              if II.compare_interval_ub_int index_itv num_elem >= 0
              then (
                let num_elem_str = pr_int num_elem in
                let reason =
                  ("Buffer at pointer '" ^ ptr_name ^ "' contains ")
                  ^ (num_elem_str ^ " elements;\n")
                  ^ "accessing index is " ^ index_itv_str in
                Some (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
              else None)
            else (
              let%bind penv_msz = dfa.dfa_env_memsize in
              let%bind fenvs_msz = Hashtbl.find penv_msz.penv_func_envs func in
              List.fold_left
                ~f:(fun acc fenv_msz ->
                  if acc != None
                  then acc
                  else (
                    let%bind data_msz = MS.get_instr_output fenv_msz ins in
                    match MS.get_size ptr data_msz with
                    | Bottom -> None
                    | Range sz ->
                      let elem_typ = LL.element_type (LL.type_of ptr) in
                      let elem_size =
                        II.Int64 (LI.size_of_type elem_typ data_layout) in
                      let max_num_elem = II.udiv_bound sz.range_ub elem_size in
                      let min_num_elem = II.udiv_bound sz.range_lb elem_size in
                      if II.compare_interval_ub_bound index_itv max_num_elem
                         >= 0
                      then (
                        let num_elem_str = II.pr_bound max_num_elem in
                        let reason =
                          ("Buffer at pointer '" ^ ptr_name ^ "' contains ")
                          ^ ("at most " ^ num_elem_str ^ " elements;\n")
                          ^ "accessing index is " ^ index_itv_str in
                        Some
                          (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
                      else if II.compare_interval_ub_bound index_itv
                                min_num_elem
                              >= 0
                      then (
                        let num_elem_str = II.pr_bound min_num_elem in
                        let reason =
                          ("Buffer at pointer '" ^ ptr_name ^ "' may contain ")
                          ^ ("only " ^ num_elem_str ^ " elements;\n")
                          ^ "accessing index is " ^ index_itv_str in
                        Some
                          (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
                      else None))
                ~init:None fenvs_msz)))
        ~init:None fenvs_rng
    | _ -> None
  ;;

  let find_bug_buffer_overflow (dfa : dfa_data) : bug list =
    if !bug_all || !bug_memory_all || !bug_buffer_overflow
    then (
      let _ = debug "Finding All Buffer Overflow Bugs..." in
      dfa.dfa_potential_bugs
      |> List.map ~f:(check_bug_buffer_overflow dfa)
      |> List.filter_opt)
    else []
  ;;

  (*--------------------
   * Check memory leak
   *-------------------*)

  let check_bug_memory_leak (dfa : dfa_data) (pbug : potential_bug)
      : bug option
    =
    match pbug.pbug_type with
    | MemoryLeak (Some mlk) ->
      let _ =
        print ~mtype:"TODO"
          "check_bug_memory_leak: IMPLEMENT CHECK MEMORY LEAK" in
      None
    | _ -> None
  ;;

  let find_bug_memory_leak (dfa : dfa_data) : bug list =
    if !bug_all || !bug_memory_all || !bug_memory_leak
    then (
      let _ = debug "Finding All Memory Leak Bugs..." in
      dfa.dfa_potential_bugs
      |> List.map ~f:(check_bug_memory_leak dfa)
      |> List.filter_opt)
    else []
  ;;
end

include MemoryBug

(*******************************************************************
 ** Find all bugs
 *******************************************************************)

(** TODO: need a mechanism to schedule analyses based on bugs:
    - Indetify the type of bugs will be checked
    - Determine which analyeses need to be performed. *)

let find_bugs (dfa : dfa_data) : bugs =
  if !find_bug
  then (
    let _ = println "Finding Bugs..." in
    let _ =
      ddebugp ~header:true "Potential bugs: " pr_potential_bugs
        dfa.dfa_potential_bugs in
    let bugs =
      find_bug_memory_leak dfa
      @ find_bug_buffer_overflow dfa
      @ find_bug_integer_overflow dfa
      @ find_bug_integer_underflow dfa
      @ find_bug_division_by_zero dfa in
    let _ = print ~mtype:"" (pr_bugs bugs) in
    (* let _ = report_bug_stats bugs in *)
    bugs)
  else []
;;
