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
module II = Int_interval

(*******************************************************************
 ** Integer bugs
 *******************************************************************)

module IntegerBug = struct
  (*-------------------
   * Integer overflow
   *------------------*)

  let check_bug_integer_overflow (pdata : program_data) (pbug : potential_bug)
      : bug option
    =
    let open Option.Let_syntax in
    match pbug.pbug_type with
    | IntegerOverflow (Some iof) ->
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
              let itv =
                RG.get_interval (LI.expr_of_llvalue iof.iof_expr) data in
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

  let find_bug_integer_overflow (pdata : program_data) =
    if !bug_all || !bug_integer_all || !bug_integer_overflow
    then (
      let _ = debug "Finding All Integer Overflow Bugs..." in
      pdata.pdata_potential_bugs
      |> List.map ~f:(check_bug_integer_overflow pdata)
      |> List.filter_opt)
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
    | IntegerUnderflow (Some iuf) ->
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
              let itv =
                RG.get_interval (LI.expr_of_llvalue iuf.iuf_expr) data in
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

  let find_bug_integer_underflow (pdata : program_data) : bugs =
    if !bug_all || !bug_integer_all || !bug_integer_underflow
    then (
      let _ = debug "Finding All Integer Underflow Bugs..." in
      pdata.pdata_potential_bugs
      |> List.map ~f:(check_bug_integer_underflow pdata)
      |> List.filter_opt)
    else []
  ;;

  (*--------------------
   * Division by Zero
   *-------------------*)

  let check_bug_division_by_zero (pdata : program_data) (pbug : potential_bug)
      : bug option
    =
    let open Option.Let_syntax in
    match pbug.pbug_type with
    | DivisionByZero None ->
      if !bug_all || !bug_integer_all || !bug_divizion_by_zero
      then (
        let _ = hdebug "Checking Potential Bug: " pr_potential_bug pbug in
        let func = LI.func_of_instr pbug.pbug_instr in
        let%bind penv_rng = pdata.pdata_env_range in
        let%bind fenvs_rng = Hashtbl.find penv_rng.penv_func_envs func in
        List.fold_left
          ~f:(fun acc fenv ->
            if acc != None
            then acc
            else (
              let%bind data = RG.get_instr_output fenv pbug.pbug_instr in
              let divisor = LI.operand pbug.pbug_instr 1 in
              let itv = RG.get_interval (LI.expr_of_llvalue divisor) data in
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

  let find_bug_division_by_zero (pdata : program_data) : bugs =
    if !bug_all || !bug_integer_all || !bug_divizion_by_zero
    then (
      let _ = debug "Finding All Division by Zero Bugs..." in
      pdata.pdata_potential_bugs
      |> List.map ~f:(check_bug_division_by_zero pdata)
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

  let check_bug_buffer_overflow (pdata : program_data) (pbug : potential_bug)
      : bug option
    =
    let open Option.Let_syntax in
    let prog = pdata.pdata_program in
    let data_layout = LI.get_program_data_layout prog in
    match pbug.pbug_type with
    | BufferOverflow (Some bof) ->
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
            let index_itv =
              RG.get_interval (LI.expr_of_llvalue bof.bof_elem_index) data_rng
            in
            if LI.is_pointer_to_array ptr
            then (
              let%bind num_elem = LI.compute_array_length ptr in
              if II.compare_interval_ub_int index_itv num_elem >= 0
              then (
                let reason =
                  ("Buffer at pointer " ^ LI.pr_value ptr)
                  ^ (" contains " ^ pr_int num_elem ^ " elements;\n")
                  ^ "accessing index is "
                  ^ RG.pr_interval_concise index_itv in
                Some (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
              else None)
            else (
              let%bind penv_msz = pdata.pdata_env_memsize in
              let%bind fenvs_msz = Hashtbl.find penv_msz.penv_func_envs func in
              List.fold_left
                ~f:(fun acc fenv_msz ->
                  if acc != None
                  then acc
                  else (
                    let%bind data_msz =
                      MS.get_instr_output fenv_msz bof.bof_instr in
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
                        let reason =
                          ("Buffer at pointer " ^ LI.pr_value ptr
                         ^ " contains ")
                          ^ ("at most " ^ II.pr_bound max_num_elem
                           ^ " elements;\n")
                          ^ "accessing index is "
                          ^ RG.pr_interval_concise index_itv in
                        Some
                          (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
                      else if II.compare_interval_ub_bound index_itv
                                min_num_elem
                              >= 0
                      then (
                        let reason =
                          ("Buffer at pointer " ^ LI.pr_value ptr
                         ^ " may contain ")
                          ^ ("only " ^ II.pr_bound min_num_elem
                           ^ " elements;\n")
                          ^ "accessing index is "
                          ^ RG.pr_interval_concise index_itv in
                        Some
                          (mk_real_bug ~checker:"RangeAnalysis" ~reason pbug))
                      else None))
                ~init:None fenvs_msz)))
        ~init:None fenvs_rng
    | _ -> None
  ;;

  let find_bug_buffer_overflow (pdata : program_data) : bug list =
    if !bug_all || !bug_memory_all || !bug_buffer_overflow
    then (
      let _ = debug "Finding All Buffer Overflow Bugs..." in
      pdata.pdata_potential_bugs
      |> List.map ~f:(check_bug_buffer_overflow pdata)
      |> List.filter_opt)
    else []
  ;;

  (*--------------------
   * Check memory leak
   *-------------------*)

  let check_bug_memory_leak (pdata : program_data) (pbug : potential_bug)
      : bug option
    =
    match pbug.pbug_type with
    | MemoryLeak (Some mlk) ->
      let _ =
        print ~marker:"TODO"
          "check_bug_memory_leak: IMPLEMENT CHECK MEMORY LEAK" in
      None
    | _ -> None
  ;;

  let find_bug_memory_leak (pdata : program_data) : bug list =
    if !bug_all || !bug_memory_all || !bug_memory_leak
    then (
      let _ = debug "Finding All Memory Leak Bugs..." in
      pdata.pdata_potential_bugs
      |> List.map ~f:(check_bug_memory_leak pdata)
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

let find_bugs (pdata : program_data) : unit =
  let _ = println "Checking Bugs..." in
  let _ =
    hddebug ~header:true "Potential bugs: " pr_potential_bugs
      pdata.pdata_potential_bugs in
  let bugs =
    find_bug_memory_leak pdata
    @ find_bug_buffer_overflow pdata
    @ find_bug_integer_overflow pdata
    @ find_bug_integer_underflow pdata
    @ find_bug_division_by_zero pdata in
  let _ = print ~marker:"" (pr_bugs bugs) in
  let _ = num_of_bugs := List.length bugs in
  report_bug_stats bugs
;;
