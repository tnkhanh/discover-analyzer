(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module LI = Llir
module BG = Bug
module LL = Llvm

type benchmark_result =
  { ben_correct_bug_reports : int;
    ben_incorrect_bug_reports : int;
    ben_missing_bugs : int;
    ben_detailed_result : string
  }

(*let dummy_ben_result =*)
(*{*)
(*ben_found_bugs = 0;*)
(*ben_missing_bugs = 0;*)
(*ben_incorrect_bug_reports = 0;*)
(*ben_detailed_result = "";*)
(*}*)

(* core function for matching bug vs assertion *)
(* TODO: Check function name *)
let match_assert (bug : BG.bug) (assert_match : LI.instr * BG.bug option) =
  let assert_call, _ = assert_match in
  (* TODO: Check out this physical equality! *)
  if LI.llvalue_of_instr bug.bug_instr == LI.operand assert_call 1
  then (assert_call, Some bug), true
  else assert_match, false
;;

let match_asserts
    (bug : BG.bug)
    (assert_matches : (LI.instr * BG.bug option) list)
  =
  let assert_results = List.map ~f:(match_assert bug) assert_matches in
  let new_assert_matches, results = List.unzip assert_results in
  new_assert_matches, List.exists ~f:(fun res -> res) results
;;

let match_groups (bug : BG.bug) assert_groups =
  let assert_results = List.map ~f:(match_asserts bug) assert_groups in
  let new_assert_groups, results = List.unzip assert_results in
  new_assert_groups, List.exists ~f:(fun res -> res) results
;;

let update_matches (assert_groups, match_count) bug =
  let new_assert_groups, result = match_groups bug assert_groups in
  new_assert_groups, if result then match_count + 1 else match_count
;;

let compute_misses_from_asserts asserts =
  let miss =
    List.for_all
      ~f:(fun (_, bug_opt) ->
        match bug_opt with
        | None -> true
        | Some _ -> false)
      asserts in
  if miss then asserts else []
;;

let compute_misses_from_groups assert_groups =
  let unfiltered_misses =
    List.map ~f:compute_misses_from_asserts assert_groups in
  List.filter
    ~f:(fun asserts ->
      match asserts with
      | [] -> false
      | _ :: _ -> true)
    unfiltered_misses
;;

let pr_bug_misses bug_misses =
  List.fold ~init:""
    ~f:(fun acc (assert_call, _) ->
      acc ^ LL.string_of_llvalue (LI.llvalue_of_instr assert_call) ^ "\n")
    bug_misses
;;

let pr_bug_miss_groups bug_miss_groups =
  List.fold ~init:""
    ~f:(fun acc bug_misses -> acc ^ pr_bug_misses bug_misses ^ "------\n")
    bug_miss_groups
;;

let pr_bug_matches (assert_groups : (LI.instr * BG.bug option) list list) =
  let assert_calls = List.concat assert_groups in
  List.fold ~init:""
    ~f:(fun acc (assert_call, bug_opt) ->
      match bug_opt with
      | None -> acc
      | Some bug ->
        let instr_str =
          LL.string_of_llvalue (LI.llvalue_of_instr assert_call) in
        acc ^ instr_str ^ "\n - " ^ bug.bug_reason ^ "\n------\n")
    assert_calls
;;

let compute_benchmark_result (prog : LI.program) (bugs : BG.bug list)
    : benchmark_result
  =
  let finstr =
    Some
      (fun acc instr ->
        if LI.is_instr_call instr
        then (
          let callee = LI.callee_of_instr_call instr in
          let func_name = LI.func_name callee in
          if String.is_prefix ~prefix:"__assert_ins" func_name
          then (
            match acc with
            | [] -> [ [ instr, None ] ]
            | hd_calls :: tl_calls ->
              (match hd_calls with
              | [] -> error "Error: This is not supposed to happen"
              | (hd_call, _) :: _ ->
                let hd_counter =
                  match LI.int64_of_const (LI.operand hd_call 0) with
                  | None -> error "Error: This is not supposed to happen"
                  | Some i -> i in
                let counter =
                  match LI.int64_of_const (LI.operand instr 0) with
                  | None -> error "Error: This is not supposed to happen"
                  | Some i -> i in
                if Int64.equal hd_counter counter
                then ((instr, None) :: hd_calls) :: tl_calls
                else [ instr, None ] :: acc))
          else acc)
        else acc) in
  (* TODO: Maybe sort the assertions. 
   * Multiple bug types for one annotation may cause problems *)
  let assert_groups = LI.visit_fold_program ~finstr [] prog in
  let _ = print "Assert calls: " in
  let _ =
    List.iter ~f:print
      (List.map ~f:(fun (c, _) -> LI.pr_instr c) (List.concat assert_groups))
  in

  let matched_asserts, correct_bug_reports =
    List.fold ~init:(assert_groups, 0) ~f:update_matches bugs in
  let incorrect_bug_reports = List.length bugs - correct_bug_reports in
  let bug_miss_groups = compute_misses_from_groups matched_asserts in
  let missing_bugs = List.length bug_miss_groups in
  let detailed_result =
    "+ Bug misses:\n"
    ^ pr_bug_miss_groups bug_miss_groups
    ^ "+ Bug matches:\n"
    ^ pr_bug_matches matched_asserts in
  { ben_correct_bug_reports = correct_bug_reports;
    ben_incorrect_bug_reports = incorrect_bug_reports;
    ben_missing_bugs = missing_bugs;
    ben_detailed_result = detailed_result
  }
;;
