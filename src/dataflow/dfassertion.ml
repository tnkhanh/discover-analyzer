(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Dfdata
open Llir
module LL = Llvm
module AS = Assertion
module PA = Pointer.Analysis
module RA = Range.Analysis

(*******************************************************************
 ** Integer assertions
 *******************************************************************)

module IntegerAssertion = struct
  let count_range_assertions (prog : LI.program) : int =
    if !assert_all || !assert_range
    then (
      let assertions =
        List.fold_left
          ~f:(fun acc func -> acc @ AS.find_range_assertions func)
          ~init:[] prog.prog_user_funcs in
      List.length assertions)
    else 0
  ;;

  let check_lower_bound
      (fenv : RA.func_env)
      (instr : instr)
      (v : value)
      (lb : int64)
      : bool
    =
    match RA.get_instr_output fenv instr with
    | None -> false
    | Some data ->
      (match RA.get_interval (expr_of_value v) data with
      | Bottom -> false
      | Range r ->
        (match r.range_lb with
        | PInf -> true
        | NInf -> false
        | Int64 i ->
          let vlb = if r.range_lb_incl then i else Int64.( + ) i Int64.one in
          Int64.( >= ) vlb lb
        | EInt i ->
          let vlb =
            if r.range_lb_incl
            then EInt.to_bint i
            else BInt.add (EInt.to_bint i) BInt.one in
          BInt.ge vlb (BInt.of_int64 lb)
        | BInt i ->
          let vlb = if r.range_lb_incl then i else BInt.add i BInt.one in
          BInt.ge vlb (BInt.of_int64 lb)))
  ;;

  let check_upper_bound (fenv : RA.func_env) instr (v : value) (ub : int64)
      : bool
    =
    match RA.get_instr_output fenv instr with
    | None -> false
    | Some data ->
      (match RA.get_interval (expr_of_value v) data with
      | Bottom -> true
      | Range r ->
        (match r.range_ub with
        | PInf -> false
        | NInf -> true
        | Int64 i ->
          let vub = if r.range_ub_incl then i else Int64.( - ) i Int64.one in
          Int64.( <= ) vub ub
        | EInt i ->
          let vub =
            if r.range_ub_incl
            then EInt.to_bint i
            else BInt.sub (EInt.to_bint i) BInt.one in
          BInt.le vub (BInt.of_int64 ub)
        | BInt i ->
          let vub = if r.range_ub_incl then i else BInt.sub i BInt.one in
          BInt.le vub (BInt.of_int64 ub)))
  ;;

  let check_lower_upper_bound
      (fenv : RA.func_env)
      instr
      (v : value)
      (lb : int64)
      (ub : int64)
      : bool
    =
    check_lower_bound fenv instr v lb && check_upper_bound fenv instr v ub
  ;;

  let check_one_range_assertion (fenvs : RA.func_env list) (ast : AS.assertion)
      : bool option
    =
    let instr = ast.AS.ast_instr in
    match ast.AS.ast_type, ast.AS.ast_predicate with
    | AS.Assert, AS.RangeLB (v, lb) ->
      let res =
        List.exists ~f:(fun fe -> check_lower_bound fe instr v lb) fenvs in
      Some res
    | AS.Assert, AS.RangeUB (v, ub) ->
      let res =
        List.exists ~f:(fun fe -> check_upper_bound fe instr v ub) fenvs in
      Some res
    | AS.Assert, AS.RangeLUB (v, lb, ub) ->
      let res =
        List.exists
          ~f:(fun fe -> check_lower_upper_bound fe instr v lb ub)
          fenvs in
      Some res
    | _ -> None
  ;;

  let check_all_range_assertions (penv : RA.prog_env) : int =
    if !assert_all || !assert_range
    then (
      let funcs = Hashtbl.keys penv.penv_func_envs in
      List.fold
        ~f:(fun acc func ->
          let assertions = AS.find_range_assertions func in
          let fenvs =
            match Hashtbl.find penv.penv_func_envs func with
            | None -> []
            | Some fenvs -> fenvs in
          let num_checked_assertions = ref 0 in
          let _ =
            List.iter
              ~f:(fun ast ->
                match check_one_range_assertion fenvs ast with
                | Some res ->
                  let _ = incr num_checked_assertions in
                  let _ =
                    if res
                    then incr num_valid_asserts
                    else incr num_invalid_asserts in
                  print_endline (AS.pr_assertion_status func ast res)
                | None -> ())
              assertions in
          acc + !num_checked_assertions)
        ~init:0 funcs)
    else 0
  ;;
end

include IntegerAssertion

(*******************************************************************
 ** Pointer assertions
 *******************************************************************)

module PointerAssertion = struct
  let count_pointer_assertions (prog : program) : int =
    let assertions =
      List.fold_left
        ~f:(fun acc func -> acc @ AS.find_alias_assertions func)
        ~init:[] prog.prog_user_funcs in
    List.length assertions
  ;;

  let check_may_alias (fenv : PA.func_env) (instr : instr) v1 v2 : bool =
    match PA.get_instr_output fenv instr with
    | None -> false
    | Some data ->
      let u1, u2 = expr_of_value v1, expr_of_value v2 in
      PA.is_may_alias_exp fenv.fenv_prog data u1 u2
  ;;

  let check_must_alias (fenv : PA.func_env) (instr : instr) v1 v2 : bool =
    let _ =
      debug
        ("Checking MustAlias in env(" ^ fenv.fenv_id ^ ")" ^ " of function: "
       ^ func_name fenv.fenv_func) in
    match PA.get_instr_output fenv instr with
    | None -> false
    | Some data ->
      let u1, u2 = expr_of_value v1, expr_of_value v2 in
      PA.is_must_alias_exp fenv.fenv_prog data u1 u2
  ;;

  let check_no_alias (fenv : PA.func_env) (instr : instr) v1 v2 : bool =
    match PA.get_instr_output fenv instr with
    | None -> false
    | Some data ->
      let u1, u2 = expr_of_value v1, expr_of_value v2 in
      PA.is_no_alias_exp fenv.fenv_prog data u1 u2
  ;;

  let check_one_pointer_assertion
      (fenvs : PA.func_env list)
      (ast : AS.assertion)
      : bool option
    =
    let instr = ast.AS.ast_instr in
    match ast.AS.ast_type with
    | AS.Assert ->
      (match ast.AS.ast_predicate with
      | AS.NoAlias (v1, v2) ->
        let res =
          List.for_all ~f:(fun fe -> check_no_alias fe instr v1 v2) fenvs in
        Some res
      | AS.MayAlias (v1, v2) ->
        let res =
          List.exists
            ~f:(fun fe ->
              if !dfa_pointer_conservative
              then check_may_alias fe instr v1 v2
              else true)
            fenvs in
        Some res
      | AS.MustAlias (v1, v2) ->
        let res =
          List.for_all ~f:(fun fe -> check_must_alias fe instr v1 v2) fenvs
        in
        Some res
      | _ -> None)
    | AS.Refute ->
      (match ast.AS.ast_predicate with
      | AS.NoAlias (v1, v2) ->
        let res =
          List.exists
            ~f:(fun fe ->
              if !dfa_pointer_conservative
              then check_may_alias fe instr v1 v2
              else true)
            fenvs in
        Some res
      | AS.MayAlias (v1, v2) ->
        let res =
          List.for_all
            ~f:(fun fe ->
              if !dfa_pointer_conservative
              then check_no_alias fe instr v1 v2
              else true)
            fenvs in
        Some res
      | AS.MustAlias (v1, v2) ->
        let res =
          List.for_all
            ~f:(fun fe ->
              check_no_alias fe instr v1 v2 || check_may_alias fe instr v1 v2)
            fenvs in
        Some res
      | _ -> None)
  ;;

  let check_all_pointer_assertions (penv : PA.prog_env) : int =
    if !assert_all || !assert_pointer
    then (
      let funcs = Hashtbl.keys penv.penv_func_envs in
      List.fold
        ~f:(fun acc func ->
          let assertions = AS.find_alias_assertions func in
          let fenvs =
            match Hashtbl.find penv.penv_func_envs func with
            | None -> []
            | Some fenvs -> fenvs in
          let num_checked_assertions = ref 0 in
          let _ =
            List.iter
              ~f:(fun ast ->
                match check_one_pointer_assertion fenvs ast with
                | Some res ->
                  let _ = incr num_checked_assertions in
                  let _ =
                    if res
                    then incr num_valid_asserts
                    else incr num_invalid_asserts in
                  print (AS.pr_assertion_status func ast res)
                | None -> ())
              assertions in
          acc + !num_checked_assertions)
        ~init:0 funcs)
    else 0
  ;;
end

include PointerAssertion

(*******************************************************************
 ** All assertions
 *******************************************************************)

let count_assertions (prog : program) : int =
  count_range_assertions prog + count_pointer_assertions prog
;;

let check_assertions (dfa : dfa_data) : unit =
  let _ = print "Checking assertions..." in
  let prog = dfa.dfa_program in
  let total_asserts = count_assertions prog in
  let checked_asserts = ref 0 in
  let _ =
    match dfa.dfa_env_range with
    | None -> ()
    | Some penv_range ->
      let range_asserts = check_all_range_assertions penv_range in
      checked_asserts := !checked_asserts + range_asserts in
  let _ =
    match dfa.dfa_env_pointer with
    | None -> ()
    | Some penv_pointer ->
      let range_asserts = check_all_pointer_assertions penv_pointer in
      checked_asserts := !checked_asserts + range_asserts in
  let unchecked_asserts = total_asserts - !checked_asserts in
  let msg =
    if total_asserts = 0
    then "No assertion is found!"
    else if unchecked_asserts == 0
    then
      sprintf "%d/%d assertion(s) are checked!\n" !checked_asserts
        total_asserts
    else
      sprintf "%d/%d assertion(s) are checked, %d are skipped!\n"
        !checked_asserts total_asserts unchecked_asserts in
  println msg
;;
