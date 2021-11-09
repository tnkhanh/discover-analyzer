(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover
open Debugger
open Slir
open Proof
open Rule
module NO = Normalize

let iprint pstate msg = if pstate.prs_interact then debugc msg else ()

(*******************************************************************
 ** rules
 *******************************************************************)

let choose_axiom_rules prog goal =
  try
    let _ = choose_rule_no_entail prog goal |> raise_first_rule in
    let _ = choose_rule_all_pure prog goal |> raise_first_rule in
    let _ = choose_rule_invalid_entail prog goal |> raise_first_rule in
    []
  with
  | ERules rs -> rs
;;

let choose_normalization_rules prog goal =
  try
    let _ = choose_rule_false_left prog goal |> raise_first_rule in
    let _ = choose_rule_valid_entails prog goal |> raise_first_rule in
    let _ = choose_rule_infer_frame prog goal |> raise_first_rule in
    let _ = choose_rule_elim_bvar prog goal |> raise_first_rule in
    let _ = choose_rule_equal_left prog goal |> raise_first_rule in
    let _ = choose_rule_exists_left prog goal |> raise_first_rule in
    let _ = choose_rule_exists_right prog goal |> raise_first_rule in
    let _ = choose_rule_reln_left prog goal |> raise_first_rule in
    let _ = choose_rule_reln_right prog goal |> raise_first_rule in
    let _ = choose_rule_wand_outer prog goal |> raise_first_rule in
    let _ = choose_rule_wand_data prog goal |> raise_first_rule in
    let _ = choose_rule_wand_inner prog goal |> raise_first_rule in
    let _ = choose_rule_wand_right prog goal |> raise_first_rule in
    (* let _ = choose_rule_data_to_array_left prog goal |> raise_first_rule in *)
    []
  with
  | ERules rs -> rs
;;

let choose_transformation_rules prog goal =
  try
    let rs = [] in
    let rs = rs @ choose_rule_unfold_head prog goal in
    let rs = rs @ choose_rule_unfold_view_left prog goal in
    let rs = rs @ choose_rule_unfold_view_right prog goal in
    let rs = rs @ choose_rule_match_data prog goal in
    let rs = rs @ choose_rule_match_view prog goal in
    let rs = rs @ choose_rule_match_array prog goal in
    let rs = rs @ choose_rule_subtract_data prog goal in
    let rs = rs @ choose_rule_empty_array_right prog goal in
    rs
  with
  | ERules rs -> rs
;;

let choose_all_rules prog goal =
  try
    let _ = choose_axiom_rules prog goal |> raise_first_rule in
    let _ = choose_normalization_rules prog goal |> raise_first_rule in
    choose_transformation_rules prog goal
  with
  | ERules rs -> rs
;;

(*******************************************************************
 ** proof search
 *******************************************************************)

let rec process_conjunctive_subgoals pstate goal rule subgoals =
  let rec prove_subgoals (sts, pts) sgs =
    match sgs with
    | [] -> sts, pts
    | g :: gs ->
      let pt = prove_one_goal pstate g in
      let npts = pts @ [ pt ] in
      (match pt.ptr_status with
      | Some true -> prove_subgoals (Some true, npts) gs
      | Some false -> Some false, npts
      | None -> None, npts) in
  let status, pts = prove_subgoals (Some true, []) subgoals in
  match status with
  | Some true -> mk_proof_tree_valid goal rule pts
  | Some false -> mk_proof_tree_invalid goal rule pts
  | _ -> mk_proof_tree_unknown goal

and process_one_derivation pstate drv : proof_tree =
  let goal, rule = drv.drv_goal, drv.drv_rule in
  match drv.drv_kind with
  | DrvStatus (Some true) -> mk_proof_tree_valid goal rule []
  | DrvStatus (Some false) -> mk_proof_tree_invalid goal rule []
  | DrvStatus _ -> mk_proof_tree_unknown goal
  | DrvSubgoals sgs -> process_conjunctive_subgoals pstate goal rule sgs

and reorder_rules pstate goal rules : rule list =
  let compare_rules r1 r2 =
    encode_prio (compare_transformation_rules pstate goal r1 r2) in
  (* sort decreasingly *)
  List.sort ~compare:(fun r1 r2 -> compare_rules r2 r1) rules

and remove_useless_rules pstate goal rules : rule list =
  List.filter ~f:(fun r -> not (is_rule_useless pstate goal rules r)) rules

and select_one_candidate_rule pstate goal rules : (rule * rule list) option =
  match pstate.prs_interact, rules with
  | true, [] -> None
  | true, _ ->
    let choices = display_choices "CANDIDATE RULES:" pr_rule rules in
    let decicion = ask_decision "Choose a rule: " [ "a"; "q"; choices ] in
    if String.equal decicion "a"
    then (
      let _ = pstate.prs_interact <- false in
      Some (List.hd_exn rules, List.tl_exn rules))
    else if String.equal decicion "q"
    then None
    else List.extract_nth (int_of_string decicion - 1) rules
  | false, [] -> None
  | false, _ -> Some (List.hd_exn rules, List.tl_exn rules)

and process_all_rules pstate goal rules : proof_tree =
  let rec process_rules ptrees rules =
    match pstate.prs_interact, rules with
    | true, [] ->
      let _ = iprint pstate "No rule is found!" in
      let decision = ask_decision "Backtracking or quit? " [ "b"; "q" ] in
      if String.equal decision "b"
      then Some (mk_proof_tree_unknown goal)
      else None
    | false, [] -> None
    | _ ->
      let _ = iprint pstate ("PROVING GOAL: \n" ^ pr_goal goal) in
      (match select_one_candidate_rule pstate goal rules with
      | None -> Some (mk_proof_tree_unknown goal)
      | Some (rule, other_rules) ->
        let drv = process_one_rule pstate goal rule in
        let ptree = process_one_derivation pstate drv in
        (match ptree.ptr_status, other_rules with
        | Some true, _ -> Some ptree
        | Some false, _ -> Some ptree
        | None, [] -> Some (mk_proof_tree_unknown goal)
        | None, _ -> process_rules ptrees other_rules)) in
  match process_rules [] rules with
  | None -> mk_proof_tree_unknown goal
  | Some pt -> pt

and prove_one_goal pstate goal : proof_tree =
  let _ = iprint pstate ("PROVING GOAL: \n" ^ pr_goal goal) in
  let prog = pstate.prs_prog in
  let rules = choose_all_rules prog goal in
  let _ = iprint pstate ("ALL CHOSEN RULES: " ^ pr_rules rules) in
  let rules = remove_useless_rules pstate goal rules in
  let _ = iprint pstate ("RULES AFTER REMOVE USELESS: " ^ pr_rules rules) in
  let rules = reorder_rules pstate goal rules in
  let _ = iprint pstate ("RULES AFTER REORDERING: " ^ pr_rules rules) in
  process_all_rules pstate goal rules
;;

let init_global_vars ents =
  let max_index = ents |> List.map ~f:(fun e -> e.ent_id) |> Math.max_ints in
  index_entail := if max_index <= 0 then 0 else max_index
;;

let dump_proof_tree pstate ptree =
  let _ = if !export_proof_ascii then Ascii.export_ptree_core ptree in
  ()
;;

let prove_entailments prog ents : bool option =
  let _ = init_global_vars ents in
  let goal = mk_goal ents in
  let pstate = mk_prover_state prog in
  let ptree = prove_one_goal pstate goal in
  let _ = dump_proof_tree pstate ptree in
  ptree.ptr_status
;;

let infer_entailment_frame prog ent : bool option * formula list =
  (* let _ = hdebugc "Entail: " sprint_ent ent in *)
  let _ = init_global_vars [ ent ] in
  let goal = mk_goal [ ent ] in
  let pstate = mk_prover_state prog in
  let ptree = prove_one_goal pstate goal in
  let _ = dump_proof_tree pstate ptree in
  (* let _ = hdebugc "  frames: " sprint_fs ptree.ptr_frames in *)
  ptree.ptr_status, ptree.ptr_frames
;;

let check_sat (prog : program) (fs : formulas) : bool option =
  (* TODO: need to implement *)
  None
;;
