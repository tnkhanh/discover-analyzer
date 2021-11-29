(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Slir
open Proof
open Normalize
module SMT = Smt.SmtSL
module SP = Set.Poly

(*******************************************************************
 ** choose rules
 *******************************************************************)

(* axiom rules *)

let choose_rule_no_entail prog goal : rule list =
  if List.is_empty goal.gl_entail_cores then [ mk_rule_no_entail () ] else []
;;

let choose_rule_all_pure prog goal : rule list =
  let pure_entails =
    List.filter
      ~f:(fun enc ->
        let lst, rst = enc.enc_lst, enc.enc_rst in
        lst.fst_is_pure && rst.fst_is_pure)
      goal.gl_entail_cores in
  if pure_entails != []
     && List.length pure_entails = List.length goal.gl_entail_cores
  then (
    let ents = get_entailments_of_goal goal in
    let res, _ = SMT.check_sat_horn ents in
    if is_bool_result_true res
    then [ mk_rule_all_pure goal.gl_entail_cores ]
    else [])
  else []
;;

let choose_rule_invalid_entail prog goal : rule list =
  try
    let enc =
      List.find_exn
        ~f:(fun enc ->
          let lhs, rhs = enc.enc_lhs, enc.enc_rhs in
          match lhs, rhs with
          | Pure plhs, Pure prhs ->
            is_bool_result_false (SMT.check_imply plhs prhs)
          | _ -> false)
        goal.gl_entail_cores in
    [ mk_rule_invalid_entail enc ]
  with _ -> []
;;

(* normalization rules *)

let choose_rule_false_left prog goal : rule list =
  let encs =
    List.filter
      ~f:(fun enc ->
        let nlhs, changed = simplify_tauto_contra_f enc.enc_lhs in
        changed && is_f_false nlhs)
      goal.gl_entail_cores in
  if List.is_empty encs then [] else [ mk_rule_false_left encs ]
;;

let choose_rule_valid_entails prog goal : rule list =
  let encs =
    List.filter
      ~f:(fun enc ->
        let lhs, rhs = enc.enc_lhs, enc.enc_rhs in
        let lst, rst = enc.enc_lst, enc.enc_rst in
        if lst.fst_is_pure && rst.fst_is_pure
        then (
          let plhs = extract_pure_form lhs in
          let prhs = extract_pure_form rhs in
          is_bool_result_true (SMT.check_imply plhs prhs))
        else false)
      goal.gl_entail_cores in
  if encs != [] then [ mk_rule_valid_entails encs ] else []
;;

let choose_rule_infer_frame prog goal : rule list =
  match goal.gl_entail_cores with
  | [ enc ] ->
    if enc.enc_rst.fst_is_pure
    then (
      let plhs = encode_formula_to_pure enc.enc_lhs in
      let prhs = extract_pure_form enc.enc_rhs in
      if is_bool_result_true (SMT.check_imply plhs prhs)
      then [ mk_rule_infer_frame enc ]
      else [])
    else []
  | _ -> []
;;

let choose_rule_elim_bvar prog goal : rule list =
  let encs_eqss =
    List.fold_left
      ~f:(fun acc enc ->
        let eqs =
          enc.enc_lhs |> collect_eq_exp_f
          |> List.filter ~f:(function
               | EqPure _ -> true
               | _ -> false) in
        if List.is_empty eqs then acc else acc @ [ enc, eqs ])
      ~init:[] goal.gl_entail_cores in
  if List.is_empty encs_eqss then [] else [ mk_rule_elim_bvar encs_eqss ]
;;

let choose_rule_equal_left prog goal : rule list =
  let encs_eqss =
    List.fold_left
      ~f:(fun acc enc ->
        let eqs =
          enc.enc_lhs |> collect_eq_exp_f
          |> List.filter ~f:(function
               | EqExp _ -> true
               | _ -> false) in
        if List.is_empty eqs then acc else acc @ [ enc, eqs ])
      ~init:[] goal.gl_entail_cores in
  if List.is_empty encs_eqss then [] else [ mk_rule_equal_left encs_eqss ]
;;

let choose_rule_exists_left prog goal : rule list =
  let can_subst_vars vs eqs =
    List.exists
      ~f:(function
        | EqExp (v, _) -> member_vs v vs
        | _ -> false)
      eqs in
  let rec need_simplify_p f =
    match f with
    | Bool _ | BExp _ | BEq _ | Reln _ -> false
    | PNeg g -> need_simplify_p g
    | PConj gs | PDisj gs -> List.exists ~f:need_simplify_p gs
    | PForall (vs, g) -> need_simplify_p g
    | PExists (vs, g) ->
      let eqs = collect_eq_exp_p g in
      can_subst_vars vs eqs in
  let rec need_simplify_f f =
    match f with
    | Pure p -> need_simplify_p p
    | Emp | Data _ | View _ | Iter _ | Array _ -> false
    | Star fs -> List.exists ~f:need_simplify_f fs
    | Wand (f1, f2) | Septract (f1, f2) ->
      need_simplify_f f1 || need_simplify_f f2
    | Exists (vs, g) ->
      need_simplify_f g || can_subst_vars vs (collect_eq_exp_f g) in
  let encs =
    List.filter
      ~f:(fun enc -> need_simplify_f enc.enc_lhs)
      goal.gl_entail_cores in
  if List.is_empty encs then [] else [ mk_rule_exists_left encs ]
;;

let choose_rule_exists_right prog goal : rule list =
  let can_subst_vars vs eqs =
    List.exists
      ~f:(function
        | EqExp (v, _) -> member_vs v vs
        | _ -> false)
      eqs in
  let rec need_simplify_p f =
    match f with
    | Bool _ | BExp _ | BEq _ | Reln _ -> false
    | PNeg g -> need_simplify_p g
    | PConj gs | PDisj gs -> List.exists ~f:need_simplify_p gs
    | PForall (vs, g) -> need_simplify_p g
    | PExists (vs, g) ->
      let eqs = collect_eq_exp_p g in
      can_subst_vars vs eqs in
  let rec need_simplify_f f =
    match f with
    | Pure p -> need_simplify_p p
    | Emp | Data _ | View _ | Iter _ | Array _ -> false
    | Star fs -> List.exists ~f:need_simplify_f fs
    | Wand (f1, f2) | Septract (f1, f2) ->
      need_simplify_f f1 || need_simplify_f f2
    | Exists (vs, g) ->
      need_simplify_f g || can_subst_vars vs (collect_eq_exp_f g) in
  let encs =
    List.filter
      ~f:(fun enc -> need_simplify_f enc.enc_rhs)
      goal.gl_entail_cores in
  if List.is_empty encs then [] else [ mk_rule_exists_right encs ]
;;

let choose_rule_reln_left prog goal : rule list =
  let rec find_rule encs =
    match encs with
    | [] -> []
    | enc :: encs ->
      let rnames = collect_reln_name_f enc.enc_lhs in
      (try
         let rdefn =
           List.find_exn
             ~f:(fun rd -> SP.exists ~f:(String.equal rd.relnd_name) rnames)
             prog.prog_reln_defns in
         [ mk_rule_reln_left rdefn enc ]
       with _ -> find_rule encs) in
  find_rule goal.gl_entail_cores
;;

let choose_rule_reln_right prog goal : rule list =
  let rec find_rule encs =
    match encs with
    | [] -> []
    | enc :: encs ->
      (match enc.enc_rhs with
      | Pure (Reln (RName rn, _)) ->
        (match find_reln_defn prog.prog_reln_defns rn with
        | None -> find_rule encs
        | Some rdefn -> [ mk_rule_reln_right rdefn enc ])
      | _ -> find_rule encs) in
  find_rule goal.gl_entail_cores
;;

(* NOTE: this rule might be unsound for a resolution-based proof system *)
let choose_rule_wand_inner prog goal : rule list =
  (* Rule: F * (F --* G)  ==>  G. The direction <== is not correct. *)
  (* TODO: prove this property in the notes folder *)
  let rec find_elim_form f =
    match f with
    | Emp | Pure _ | Data _ | View _ | Iter _ | Array _ -> []
    | Wand (f1, f2) -> find_elim_form f1 @ find_elim_form f2
    | Septract _ -> [] (* FIXME: need to handle this *)
    | Star fs ->
      let fs1, fs2 = List.partition_tf ~f:is_f_data_or_view fs in
      let gs =
        try
          let f =
            List.find_exn
              ~f:(fun f1 ->
                List.exists
                  ~f:(function
                    | Wand (f2, _) ->
                      equal_df_form f1 f2 || equal_vf_form f1 f2
                    | _ -> false)
                  fs2)
              fs1 in
          [ f ]
        with _ -> [] in
      List.fold_left ~f:(fun acc f -> acc @ find_elim_form f) ~init:gs fs
    | Exists (_, g) -> find_elim_form g in
  let encs =
    List.fold_left
      ~f:(fun acc e ->
        match find_elim_form e.enc_lhs with
        | [] -> acc
        | fs -> acc @ [ e, fs ])
      ~init:[] goal.gl_entail_cores in
  if List.is_empty encs then [] else [ mk_rule_wand_inner encs ]
;;

let choose_rule_wand_outer prog goal : rule list =
  (* Rule: F --* (F * G)  <==>  G. Use currying and decurrying property *)
  (* TODO: prove this property in the notes folder *)
  let rec find_elim_form f =
    match f with
    | Emp | Pure _ | Data _ | View _ | Iter _ | Array _ -> []
    | Star fs ->
      List.fold_left ~f:(fun acc f -> acc @ find_elim_form f) ~init:[] fs
    | Wand ((Data df as fd), g) ->
      let fs =
        match g with
        | Star fs -> fs
        | _ -> [ g ] in
      let gs, _ = List.partition_tf ~f:(equal_df_form fd) fs in
      List.fold_left ~f:(fun acc f -> acc @ find_elim_form f) ~init:gs fs
    | Wand ((View vf as fv), g) ->
      let fs =
        match g with
        | Star fs -> fs
        | _ -> [ g ] in
      let gs, _ = List.partition_tf ~f:(equal_vf_form fv) fs in
      List.fold_left ~f:(fun acc f -> acc @ find_elim_form f) ~init:gs fs
    | Wand (f1, f2) -> find_elim_form f1 @ find_elim_form f2
    | Septract _ -> [] (* FIXME: need to handle this *)
    | Exists (_, g) -> find_elim_form g in
  let encs =
    List.fold_left
      ~f:(fun acc e ->
        match find_elim_form e.enc_lhs with
        | [] -> acc
        | fs -> acc @ [ e, fs ])
      ~init:[] goal.gl_entail_cores in
  if List.is_empty encs then [] else [ mk_rule_wand_outer encs ]
;;

let choose_rule_wand_data prog goal : rule list =
  (* Rule: x->y --* (x->z * G)  <==>  G & y=z *)
  let rec find_elim_form f =
    match f with
    | Emp | Pure _ | Data _ | View _ | Iter _ | Array _ -> []
    | Star fs ->
      List.fold_left ~f:(fun acc f -> acc @ find_elim_form f) ~init:[] fs
    | Wand ((Data df as fd), g) ->
      let fs =
        match g with
        | Star fs -> fs
        | _ -> [ g ] in
      let gs, _ = List.partition_tf ~f:(equal_df_form_name_root fd) fs in
      List.fold_left ~f:(fun acc f -> acc @ find_elim_form f) ~init:gs fs
    | Wand (f1, f2) -> find_elim_form f1 @ find_elim_form f2
    | Septract _ -> [] (* FIXME: need to handle this *)
    | Exists (_, g) -> find_elim_form g in
  let encs =
    List.fold_left
      ~f:(fun acc e ->
        match find_elim_form e.enc_lhs with
        | [] -> acc
        | fs -> acc @ [ e, fs ])
      ~init:[] goal.gl_entail_cores in
  if List.is_empty encs then [] else [ mk_rule_wand_data encs ]
;;

let choose_rule_wand_right prog goal : rule list =
  (* Rule: P |- Q --* R  <==>  P * Q |- R *)
  let encs =
    List.filter
      ~f:(fun e ->
        match e.enc_rhs with
        | Wand _ -> true
        | _ -> false)
      goal.gl_entail_cores in
  List.map ~f:mk_rule_wand_right encs
;;

(* let choose_rule_data_to_array_left prog goal : rule list =
 *   List.fold_left ~f:(fun arules enc ->
 *     let rules =
 *       let lst, rst = enc.enc_lst, enc.enc_rst in
 *       let ldsps = lst.fst_data_splits in
 *       let rasps = rst.fst_array_splits in
 *       List.fold_left ~f:(fun acc ldsp ->
 *         let ldf, lrest = ldsp.dsp_head, ldsp.dsp_rest in
 *         if List.exists ~f:(fun rasp ->
 *           equal_exp ldf.data_root rasp.asp_head.array_root) rasps then
 *           acc @ [mk_rule_data_to_array_left ldf lrest enc]
 *         else acc) ~init:[] ldsps in
 *     arules @ rules) ~init:[] goal.gl_entail_cores *)

(* transformation rules *)

let choose_rule_unfold_head prog goal : rule list =
  let rec find_rule encs used_encs rs =
    match encs with
    | [] -> rs
    | enc :: nencs ->
      let nused_encs = used_encs @ [ enc ] in
      let oencs = nencs @ used_encs in
      (match enc.enc_rhs with
      | Pure (Reln (RName rn, args)) ->
        let rns_lhs, rns_rhs =
          List.fold_left
            ~f:(fun (bs, hs) e ->
              let bs = SP.union_list [ bs; collect_reln_name_f e.enc_lhs ] in
              let hs = SP.union_list [ hs; collect_reln_name_f e.enc_rhs ] in
              bs, hs)
            ~init:(SP.empty, SP.empty) oencs in
        let rs =
          if not (SP.mem rns_lhs rn)
          then rs
          else if SP.mem rns_rhs rn
          then rs
          else rs @ [ mk_rule_unfold_head enc ] in
        find_rule nencs nused_encs rs
      | View v ->
        let vns_lhs, vns_rhs =
          List.fold_left
            ~f:(fun (bs, hs) e ->
              let bs = SP.union_list [ bs; collect_view_name_f e.enc_lhs ] in
              let hs = SP.union_list [ hs; collect_view_name_f e.enc_rhs ] in
              bs, hs)
            ~init:(SP.empty, SP.empty) oencs in
        let rs =
          if not (SP.mem vns_lhs v.view_name)
          then rs
          else if SP.mem vns_rhs v.view_name
          then rs
          else rs @ [ mk_rule_unfold_head enc ] in
        find_rule nencs nused_encs rs
      | _ -> find_rule nencs nused_encs rs) in
  find_rule goal.gl_entail_cores [] []
;;

let choose_rule_unfold_view_left prog goal : rule list =
  List.fold
    ~f:(fun acc1 enc ->
      List.fold
        ~f:(fun acc2 vsp ->
          let vf, rest, evs = vsp.vsp_head, vsp.vsp_rest, vsp.vsp_evars in
          if has_view_defn prog vsp.vsp_head.view_name
          then acc2 @ [ mk_rule_unfold_view_left vf rest evs enc ]
          else acc2)
        ~init:acc1 enc.enc_lst.fst_view_splits)
    ~init:[] goal.gl_entail_cores
;;

let choose_rule_unfold_view_right prog goal : rule list =
  List.fold
    ~f:(fun arules enc ->
      List.fold
        ~f:(fun acc vsp ->
          let vf, rest, evs = vsp.vsp_head, vsp.vsp_rest, vsp.vsp_evars in
          if has_view_defn prog vsp.vsp_head.view_name
          then acc @ [ mk_rule_unfold_view_left vf rest evs enc ]
          else acc)
        ~init:arules enc.enc_rst.fst_view_splits)
    ~init:[] goal.gl_entail_cores
;;

let choose_rule_match_data prog goal : rule list =
  List.fold_left
    ~f:(fun arules enc ->
      List.fold_left
        ~f:(fun acc1 ldsp ->
          List.fold_left
            ~f:(fun acc2 rdsp ->
              let ldf, rdf = ldsp.dsp_head, rdsp.dsp_head in
              if equal_typ ldf.data_typ rdf.data_typ
              then acc2 @ [ mk_rule_match_data enc ldsp rdsp ]
              else acc2)
            ~init:acc1 enc.enc_rst.fst_data_splits)
        ~init:arules enc.enc_lst.fst_data_splits)
    ~init:[] goal.gl_entail_cores
;;

let choose_rule_match_view prog goal : rule list =
  List.fold_left
    ~f:(fun arules enc ->
      List.fold_left
        ~f:(fun acc1 lvsp ->
          List.fold_left
            ~f:(fun acc2 rvsp ->
              let ldf, rdf = lvsp.vsp_head, rvsp.vsp_head in
              if String.equal ldf.view_name rdf.view_name
              then acc2 @ [ mk_rule_match_view enc lvsp rvsp ]
              else acc2)
            ~init:acc1 enc.enc_rst.fst_view_splits)
        ~init:arules enc.enc_lst.fst_view_splits)
    ~init:[] goal.gl_entail_cores
;;

let choose_rule_match_array prog goal : rule list =
  List.fold_left
    ~f:(fun arules enc ->
      List.fold_left
        ~f:(fun acc1 lasp ->
          List.fold_left
            ~f:(fun acc2 rasp ->
              let laf, raf = lasp.asp_head, rasp.asp_head in
              if equal_exp laf.array_root raf.array_root
              then acc2 @ [ mk_rule_match_array enc lasp rasp ]
              else acc2)
            ~init:acc1 enc.enc_rst.fst_array_splits)
        ~init:arules enc.enc_lst.fst_array_splits)
    ~init:[] goal.gl_entail_cores
;;

let choose_rule_subtract_data prog goal : rule list =
  List.fold_left
    ~f:(fun arules enc ->
      List.fold_left
        ~f:(fun acc1 lasp ->
          let laf = lasp.asp_head in
          List.fold_left
            ~f:(fun acc2 rdsp ->
              let rdf = rdsp.dsp_head in
              let has_same_root_df =
                let update_cond =
                  List.exists
                    ~f:(fun d -> equal_exp d.data_root rdsp.dsp_head.data_root)
                    laf.array_update in
                let root_cond = equal_exp laf.array_root rdf.data_root in
                update_cond || root_cond in
              if has_same_root_df
              then acc2 @ [ mk_rule_subtract_data enc lasp rdsp ]
              else acc2)
            ~init:acc1 enc.enc_rst.fst_data_splits)
        ~init:arules enc.enc_lst.fst_array_splits)
    ~init:[] goal.gl_entail_cores
;;

let choose_rule_empty_array_right prog goal : rule list =
  List.fold_left
    ~f:(fun arules enc ->
      List.fold_left
        ~f:(fun acc rasp ->
          let raf, rest, evs = rasp.asp_head, rasp.asp_rest, rasp.asp_evars in
          let has_data_same_root =
            List.exists
              ~f:(fun ldsp -> equal_exp ldsp.dsp_head.data_root raf.array_root)
              enc.enc_lst.fst_data_splits in
          let has_array_same_root =
            List.exists
              ~f:(fun lasp ->
                equal_exp lasp.asp_head.array_root raf.array_root)
              enc.enc_lst.fst_array_splits in
          if (not has_data_same_root) && not has_array_same_root
          then acc @ [ mk_rule_empty_array_right enc raf rest evs ]
          else acc)
        ~init:arules enc.enc_rst.fst_array_splits)
    ~init:[] goal.gl_entail_cores
;;

(*******************************************************************
 ** process rules
 *******************************************************************)

let pr_rule_result ?(print = true) pstate rule encs =
  let action = pr_rule rule in
  if pstate.prs_interact && print
  then (
    let sencs =
      encs
      |> List.map ~f:(fun c -> "  " ^ pr_enc ~id:true c)
      |> String.concat ~sep:"\n  ==>\n" in
    debug ("@ " ^ action ^ ":\n" ^ sencs))
  else ()
;;

(*** axiom rules ***)

let process_rule_no_entail pstate goal : derivation =
  let rule = RlNoEntail in
  mk_derivation_valid goal rule
;;

let process_rule_all_pure pstate goal r : derivation =
  let rule = RlAllPure r in
  mk_derivation_valid goal rule
;;

let process_rule_invalid_entail pstate goal r : derivation =
  let rule = RlInvalidEntail r in
  mk_derivation_invalid goal rule
;;

(*** normalization rules ***)

let process_rule_false_left pstate goal r : derivation =
  let rule = RlFalseLeft r in
  let nencs =
    List.map
      ~f:(fun enc ->
        if List.exists ~f:(fun h -> h.enc_id = enc.enc_id) r.rfl_entails
        then update_entail_core enc ~lhs:[ mk_f_false () ]
        else enc)
      goal.gl_entail_cores in
  let sub_goal = update_goal_entcores goal nencs in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_valid_entails pstate goal r : derivation =
  let rule = RlValidEntails r in
  let sub_goal = remove_goal_entcores goal r.rve_entails in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_infer_frame pstate goal r : derivation =
  let rule = RlInferFrame r in
  let sub_goal = remove_goal_entcores goal [ r.rif_entail ] in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_elim_bvar pstate goal r : derivation =
  let rule = RlElimBVar r in
  let rec elim_p (sst : var -> pure_form) p =
    match p with
    | Bool _ | Reln _ -> p
    | BExp (Var v) -> sst v
    | BExp _ -> p
    | BEq (e, p) -> mk_pf_true ()
    | PNeg f -> f |> elim_p sst |> mk_pneg
    | PConj fs -> fs |> List.map ~f:(elim_p sst) |> mk_pconj
    | PDisj fs -> fs |> List.map ~f:(elim_p sst) |> mk_pdisj
    | PForall (vs, f) -> mk_pforall vs (elim_p sst f)
    | PExists (vs, f) -> mk_pexists vs (elim_p sst f)
  and elim_f (sst : var -> pure_form) f =
    match f with
    | Emp | Data _ | View _ | Iter _ | Array _ -> f
    | Pure p -> mk_f_pure (elim_p sst p)
    | Star fs -> mk_f_star (List.map ~f:(elim_f sst) fs)
    | Wand (f1, f2) -> mk_f_wand (elim_f sst f1) (elim_f sst f2)
    | Septract (f1, f2) -> mk_f_septract (elim_f sst f1) (elim_f sst f2)
    | Exists (vs, f) -> mk_f_exists vs (elim_f sst f) in
  let nencs =
    List.map
      ~f:(fun (enc, eqs) ->
        let sst0 v = mk_bexp (mk_exp_var v) in
        let sst =
          List.fold_left
            ~f:(fun acc eq ->
              match eq with
              | EqExp _ -> acc
              | EqPure (v, p) -> fun u -> if equal_var u v then p else acc u)
            ~init:sst0 eqs in
        let lhs = enc.enc_lhs |> elim_f sst |> simplify_tauto_contra_f |> fst in
        let nenc = update_entail_core enc ~lhs:[ lhs ] in
        pr_rule_result pstate rule [ enc; nenc ];
        nenc)
      r.reb_entails_eqss in
  let sub_goal = update_goal_entcores goal nencs in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_equal_left pstate goal r : derivation =
  let rule = RlEqualLeft r in
  let nencs =
    List.map
      ~f:(fun (enc, eqs) ->
        let ssts = get_substitute_formula_eqs eqs in
        let lhs =
          enc.enc_lhs |> substitute_formula ssts |> simplify_arith_f
          |> simplify_tauto_contra_f |> fst in
        let rhs = enc.enc_rhs |> substitute_formula ssts in
        let nenc = update_entail_core enc ~lhs:[ lhs ] ~rhs:[ rhs ] in
        pr_rule_result pstate rule [ enc; nenc ];
        nenc)
      r.reql_entails_eqss in
  let sub_goal = update_goal_entcores goal nencs in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_exists_left pstate goal r : derivation =
  let rule = RlExistsLeft r in
  let encs =
    List.map
      ~f:(fun enc ->
        let lhs = eliminate_exists_var_by_equality_f enc.enc_lhs in
        let nenc = update_entail_core enc ~lhs:[ lhs ] in
        pr_rule_result pstate rule [ enc; nenc ];
        nenc)
      r.rexl_entails in
  let sub_goal = update_goal_entcores goal encs in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_exists_right pstate goal r : derivation =
  let rule = RlExistsRight r in
  let encs =
    List.map
      ~f:(fun enc ->
        let rhs = eliminate_exists_var_by_equality_f enc.enc_rhs in
        let nenc = update_entail_core enc ~rhs:[ rhs ] in
        pr_rule_result pstate rule [ enc; nenc ];
        nenc)
      r.rexr_entails in
  let sub_goal = update_goal_entcores goal encs in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_reln_left pstate goal r : derivation =
  let rule = RlRelnLeft r in
  let rd, renc = r.rrl_rdefn, r.rrl_entail in
  let rec process encs acc =
    match encs with
    | [] -> acc
    | enc :: encs ->
      let enc =
        if enc.enc_id = renc.enc_id
        then (
          let rhead =
            let rel = RName rd.relnd_name in
            let rargs = List.map ~f:mk_exp_var rd.relnd_params in
            mk_reln rel rargs in
          let rbody =
            match rd.relnd_body with
            | None ->
              error ("process_rule_reln_left: unknown reln: " ^ rd.relnd_name)
            | Some pf -> pf in
          let lhs = replace_reln_form_f rhead rbody enc.enc_lhs in
          update_entail_core enc ~lhs:[ lhs ])
        else enc in
      process encs (acc @ [ enc ]) in
  let encs = process goal.gl_entail_cores [] in
  let sub_goal = { goal with gl_entail_cores = encs } in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_reln_right pstate goal r : derivation =
  let rule = RlRelnRight r in
  let rd, renc = r.rrr_rdefn, r.rrr_entail in
  let rec process encs acc =
    match encs with
    | [] -> acc
    | enc :: encs ->
      let enc =
        if enc.enc_id = renc.enc_id
        then (
          let rhead =
            let rel = RName rd.relnd_name in
            let rargs = List.map ~f:mk_exp_var rd.relnd_params in
            mk_reln rel rargs in
          let rbody =
            match rd.relnd_body with
            | None ->
              error ("process_rule_reln_left: unknown reln: " ^ rd.relnd_name)
            | Some pf -> pf in
          let rhs = replace_reln_form_f rhead rbody enc.enc_rhs in
          update_entail_core enc ~rhs:[ rhs ])
        else enc in
      process encs (acc @ [ enc ]) in
  let encs = process goal.gl_entail_cores [] in
  let sub_goal = { goal with gl_entail_cores = encs } in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_wand_inner pstate goal r : derivation =
  (* F * (F --* G)  ==>  G *)
  let rule = RlWandInner r in
  let rec eliminate f =
    match f with
    | Emp | Pure _ | Data _ | View _ | Iter _ | Array _ -> f
    | Star fs ->
      let fs1, fs2 =
        fs |> List.map ~f:eliminate |> List.partition_tf ~f:is_f_data_or_view
      in
      let rec remove fs acc =
        match fs with
        | [] -> mk_f_star (acc @ fs @ fs2)
        | f :: fs ->
          let fs21, fs22 =
            List.partition_tf
              ~f:(function
                | Wand (f2, _) -> equal_df_form f f2 || equal_vf_form f f2
                | _ -> false)
              fs2 in
          (match fs21 with
          | Wand (_, g) :: fs21 -> mk_f_star (acc @ fs @ [ g ] @ fs21 @ fs22)
          | _ -> remove fs (acc @ [ f ])) in
      remove fs1 []
    | Wand (f1, f2) -> mk_f_wand (eliminate f1) (eliminate f2)
    | Septract (f1, f2) -> mk_f_septract (eliminate f1) (eliminate f2)
    | Exists (vs, f) -> mk_f_exists vs (eliminate f) in
  let encs =
    List.map
      ~f:(fun (enc, fs) ->
        let lhs = eliminate enc.enc_lhs in
        let nenc = update_entail_core enc ~lhs:[ lhs ] in
        pr_rule_result pstate rule [ enc; nenc ];
        nenc)
      r.rwi_entails in
  let sub_goal = update_goal_entcores goal encs in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_wand_outer pstate goal r : derivation =
  (* Rule: F --* (F * G)  <==>  G *)
  let rule = RlWandOuter r in
  let rec eliminate f =
    match f with
    | Emp | Pure _ | Data _ | View _ | Iter _ | Array _ -> f
    | Wand (f1, f2) ->
      let f1, f2 = eliminate f1, eliminate f2 in
      (match f1 with
      | Data df as fd ->
        let fs =
          match f2 with
          | Star fs -> fs
          | _ -> [ f2 ] in
        let fs1, fs2 = List.partition_tf ~f:(equal_df_form fd) fs in
        if List.is_empty fs1 then f else mk_f_star (List.tl_exn fs1 @ fs2)
      | View vf as fv ->
        let fs =
          match f2 with
          | Star fs -> fs
          | _ -> [ f2 ] in
        let fs1, fs2 = List.partition_tf ~f:(equal_vf_form fv) fs in
        if List.is_empty fs1 then f else mk_f_star (List.tl_exn fs1 @ fs2)
      | _ -> mk_f_wand f1 f2)
    | Septract (f1, f2) -> mk_f_septract (eliminate f1) (eliminate f2)
    | Star fs -> fs |> List.map ~f:eliminate |> mk_f_star
    | Exists (vs, f) -> mk_f_exists vs (eliminate f) in
  let encs =
    List.map
      ~f:(fun (enc, _) ->
        let lhs = eliminate enc.enc_lhs in
        let nenc = update_entail_core enc ~lhs:[ lhs ] in
        pr_rule_result pstate rule [ enc; nenc ];
        nenc)
      r.rwo_entails in
  let sub_goal = update_goal_entcores goal encs in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_wand_data pstate goal r : derivation =
  (* Rule: x->y --* (x->z * F)  <==> F & y=z *)
  let rule = RlWandData r in
  let rec eliminate f =
    match f with
    | Emp | Pure _ | Data _ | View _ | Iter _ | Array _ -> f
    | Wand (f1, f2) ->
      let f1, f2 = eliminate f1, eliminate f2 in
      (match f1 with
      | Data df as fd ->
        let fs =
          match f2 with
          | Star fs -> fs
          | _ -> [ f2 ] in
        let fs1, fs2 = List.partition_tf ~f:(equal_df_form_name_root fd) fs in
        (match fs1 with
        | Data df1 :: nfs1 ->
          let eq_args = mk_eq_exps_pair df.data_args df1.data_args in
          mk_f_star (nfs1 @ fs2 @ [ mk_f_pure eq_args ])
        | _ -> mk_f_wand f1 f2)
      | _ -> mk_f_wand f1 f2)
    | Septract (f1, f2) -> mk_f_septract (eliminate f1) (eliminate f2)
    | Star fs -> fs |> List.map ~f:eliminate |> mk_f_star
    | Exists (vs, f) -> f |> eliminate |> mk_f_exists vs in
  let encs =
    List.map
      ~f:(fun (enc, _) ->
        let lhs = eliminate enc.enc_lhs in
        let nenc = update_entail_core enc ~lhs:[ lhs ] in
        pr_rule_result pstate rule [ enc; nenc ];
        nenc)
      r.rwd_entails in
  let sub_goal = update_goal_entcores goal encs in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_wand_right pstate goal r : derivation =
  (* Rule: P |- Q --* R  <==>  P * Q |- R *)
  let rule = RlWandRight r in
  let enc = r.rwr_entail in
  let nenc =
    match enc.enc_rhs with
    | Wand (f1, f2) ->
      let nlhs = mk_f_star_with enc.enc_lhs ~fs:[ f1 ] in
      let nrhs = f2 in
      mk_entail_core ~id:enc.enc_id nlhs nrhs
    | _ -> enc in
  let sub_goal = update_goal_entcores goal [ nenc ] in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

(* let process_rule_data_to_array_left pstate goal r : derivation =
 *   let rule, prog = RlDataToArrayLeft r, pstate.prs_prog in
 *   let rec process encs acc = match encs with
 *     | [] -> acc
 *     | enc::encs ->
 *       if enc.enc_id = r.rdtal_entail.enc_id then
 *         let df = r.rdtal_lhs_data in
 *         let aroot = df.data_root in
 *         let ac = mk_addr ~loc:df.data_loc aroot aroot (mk_exp_int 0)
 *                    (List.hd_exn df.data_args) in
 *         let af =
 *           let asize = mk_exp_int 1 in
 *           let atype = typ_of_exp df.data_root in
 *           mk_array aroot asize atype in
 *         let nf = mk_fupdate (mk_f_array af) (mk_facell ac) in
 *         let lhs = mk_f_star_with r.rdtal_lhs_rest ~afs:[af] in
 *         let nenc = update_entail_core enc ~lhs:[lhs] in
 *         acc @ [nenc] @ encs
 *       else process encs (acc @ [enc]) in
 *   let encs = process goal.gl_entail_cores [] in
 *   let sub_goal = {goal with gl_entail_cores = encs} in
 *   mk_derivation_subgoals goal [sub_goal] rule *)

(*** transformation rules ***)

let process_rule_unfold_head pstate goal r : derivation =
  let rule = RlUnfoldHead r in
  let renc = r.ruh_entail in
  let rec process encs acc =
    match encs with
    | [] -> acc
    | enc :: nencs when enc.enc_id = renc.enc_id -> process nencs acc
    | enc :: nencs ->
      let lhs, changed =
        match renc.enc_rhs with
        | View vf ->
          let vnames = collect_view_name_f enc.enc_lhs in
          if not (SP.mem vnames vf.view_name)
          then enc.enc_lhs, false
          else replace_view_form_f vf renc.enc_lhs enc.enc_lhs, true
        | Pure (Reln ((RName rn, _) as rf)) ->
          (match renc.enc_lhs with
          | Pure p ->
            let rnames = collect_reln_name_f enc.enc_lhs in
            if not (SP.mem rnames rn)
            then enc.enc_lhs, false
            else replace_reln_form_f rf p enc.enc_lhs, true
          | f -> herror "rule_unfold_head: not pure reln body" pr_f f)
        | _ -> renc.enc_lhs, false in
      let nenc = update_entail_core enc ~lhs:[ lhs ] in
      pr_rule_result ~print:changed pstate rule [ renc; enc; nenc ];
      process nencs (acc @ [ nenc ]) in
  let encs = process goal.gl_entail_cores [] in
  let sub_goal = { goal with gl_entail_cores = encs } in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_unfold_view_left pstate goal r : derivation =
  (* FIXME: re-implemenc this rule *)
  let rule, prog = RlUnfoldViewLeft r, pstate.prs_prog in
  mk_derivation_unknown goal rule
;;

let process_rule_unfold_view_right pstate goal r : derivation =
  (* FIXME: re-implemenc this rule *)
  let rule, prog = RlUnfoldViewRight r, pstate.prs_prog in
  mk_derivation_unknown goal rule
;;

let process_rule_empty_array_right pstate goal r : derivation =
  let rule, prog = RlEmptyArrayRight r, pstate.prs_prog in
  let renc, raf = r.rear_entail, r.rear_rhs_array in
  let rec process encs acc =
    match encs with
    | [] -> acc
    | enc :: encs ->
      if enc.enc_id = renc.enc_id
      then (
        let rhs =
          r.rear_rhs_rest
          |> mk_f_star_with ~pfs:[ r.rear_empty_form ]
          |> mk_f_exists r.rear_rhs_evs in
        let nenc = update_entail_core enc ~rhs:[ rhs ] in
        acc @ [ nenc ] @ encs)
      else process encs (acc @ [ enc ]) in
  let encs = process goal.gl_entail_cores [] in
  let sub_goal = { goal with gl_entail_cores = encs } in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_match_data pstate goal r : derivation =
  let rule, prog = RlMatchData r, pstate.prs_prog in
  let rec process encs acc =
    match encs with
    | [] -> acc
    | enc :: encs ->
      if enc.enc_id = r.rmd_entail.enc_id
      then (
        let rhs =
          r.rmd_rhs_rest
          |> mk_f_star_with ~pfs:[ r.rmd_matching_form ]
          |> mk_f_exists r.rmd_rhs_evars in
        let nenc =
          update_entail_core enc ~lhs:[ r.rmd_lhs_rest ] ~rhs:[ rhs ] in
        acc @ [ nenc ] @ encs)
      else process encs (acc @ [ enc ]) in
  let encs = process goal.gl_entail_cores [] in
  let sub_goal = { goal with gl_entail_cores = encs } in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_match_view pstate goal r : derivation =
  let rule, prog = RlMatchView r, pstate.prs_prog in
  let rec process encs acc =
    match encs with
    | [] -> acc
    | enc :: encs ->
      if enc.enc_id = r.rmv_entail.enc_id
      then (
        let rhs =
          r.rmv_rhs_rest
          |> mk_f_star_with ~pfs:[ r.rmv_matching_form ]
          |> mk_f_exists r.rmv_rhs_evars in
        let nenc =
          update_entail_core enc ~lhs:[ r.rmv_lhs_rest ] ~rhs:[ rhs ] in
        acc @ [ nenc ] @ encs)
      else process encs (acc @ [ enc ]) in
  let encs = process goal.gl_entail_cores [] in
  let sub_goal = { goal with gl_entail_cores = encs } in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_match_array pstate goal r : derivation =
  let rule, prog = RlMatchArray r, pstate.prs_prog in
  let rec process encs acc =
    match encs with
    | [] -> acc
    | enc :: encs ->
      if enc.enc_id = r.rma_entail.enc_id
      then (
        let rhs =
          r.rma_rhs_rest
          |> mk_f_star_with ~pfs:[ r.rma_matching_form ]
          |> mk_f_exists r.rma_rhs_evars in
        let nenc =
          update_entail_core enc ~lhs:[ r.rma_lhs_rest ] ~rhs:[ rhs ] in
        acc @ [ nenc ] @ encs)
      else process encs (acc @ [ enc ]) in
  let encs = process goal.gl_entail_cores [] in
  let sub_goal = { goal with gl_entail_cores = encs } in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

let process_rule_subtract_data pstate goal r : derivation =
  let rule, prog = RlSubtractData r, pstate.prs_prog in
  let rec process encs acc =
    match encs with
    | [] -> acc
    | enc :: encs ->
      if enc.enc_id = r.rsd_entail.enc_id
      then (
        let naf, mf = r.rsd_lhs_new_array, r.rsd_matching_form in
        let lhs = r.rsd_lhs_rest |> mk_f_star_with ~afs:[ naf ] in
        let rhs =
          r.rsd_rhs_rest
          |> mk_f_star_with ~pfs:[ mf ]
          |> mk_f_exists r.rsd_rhs_evars in
        let nenc = update_entail_core enc ~lhs:[ lhs ] ~rhs:[ rhs ] in
        acc @ [ nenc ] @ encs)
      else process encs (acc @ [ enc ]) in
  let encs = process goal.gl_entail_cores [] in
  let sub_goal = { goal with gl_entail_cores = encs } in
  mk_derivation_subgoals goal [ sub_goal ] rule
;;

(*** process all rules ***)

let process_one_rule pstate goal rule : derivation =
  match rule with
  (* axiom rules *)
  | RlNoEntail -> process_rule_no_entail pstate goal
  | RlAllPure r -> process_rule_all_pure pstate goal r
  | RlInvalidEntail r -> process_rule_invalid_entail pstate goal r
  (* normalization rules *)
  | RlFalseLeft r -> process_rule_false_left pstate goal r
  | RlValidEntails r -> process_rule_valid_entails pstate goal r
  | RlInferFrame r -> process_rule_infer_frame pstate goal r
  | RlElimBVar r -> process_rule_elim_bvar pstate goal r
  | RlEqualLeft r -> process_rule_equal_left pstate goal r
  | RlExistsLeft r -> process_rule_exists_left pstate goal r
  | RlExistsRight r -> process_rule_exists_right pstate goal r
  | RlRelnLeft r -> process_rule_reln_left pstate goal r
  | RlRelnRight r -> process_rule_reln_right pstate goal r
  | RlWandInner r -> process_rule_wand_inner pstate goal r
  | RlWandOuter r -> process_rule_wand_outer pstate goal r
  | RlWandData r -> process_rule_wand_data pstate goal r
  | RlWandRight r -> process_rule_wand_right pstate goal r
  (* | RlDataToArrayLeft r -> process_rule_data_to_array_left pstate goal r *)
  (* transformation rules *)
  | RlUnfoldHead r -> process_rule_unfold_head pstate goal r
  | RlUnfoldViewLeft r -> process_rule_unfold_view_left pstate goal r
  | RlUnfoldViewRight r -> process_rule_unfold_view_right pstate goal r
  | RlEmptyArrayRight r -> process_rule_empty_array_right pstate goal r
  | RlMatchData r -> process_rule_match_data pstate goal r
  | RlMatchView r -> process_rule_match_view pstate goal r
  | RlMatchArray r -> process_rule_match_array pstate goal r
  | RlSubtractData r -> process_rule_subtract_data pstate goal r
;;

(*******************************************************************
 ** compare transformation rules
 *******************************************************************)

(*** compare rule unfold head with others ***)

let compare_rule_unfold_head_vs_unfold_head pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_unfold_head_vs_unfold_view_left pstate goal r1 r2 =
  try (* default *)
      PrioHigh with EPrio p -> p
;;

let compare_rule_unfold_head_vs_unfold_view_right pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_unfold_head_vs_match_data pstate goal r1 r2 =
  try
    if r2.rmd_same_root then raise_prio PrioLow;
    (* default *)
    PrioEqual
  with EPrio p -> p
;;

let compare_rule_unfold_head_vs_match_view pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_unfold_head_vs_match_array pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_unfold_head_vs_subtract_data pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_unfold_head_vs_empty_array_right pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_unfold_head_vs_others pstate goal r1 r2 =
  match r2 with
  | RlUnfoldHead r2 ->
    compare_rule_unfold_head_vs_unfold_head pstate goal r1 r2
  | RlUnfoldViewLeft r2 ->
    compare_rule_unfold_head_vs_unfold_view_left pstate goal r1 r2
  | RlUnfoldViewRight r2 ->
    compare_rule_unfold_head_vs_unfold_view_right pstate goal r1 r2
  | RlMatchData r2 -> compare_rule_unfold_head_vs_match_data pstate goal r1 r2
  | RlMatchView r2 -> compare_rule_unfold_head_vs_match_view pstate goal r1 r2
  | RlMatchArray r2 ->
    compare_rule_unfold_head_vs_match_array pstate goal r1 r2
  | RlSubtractData r2 ->
    compare_rule_unfold_head_vs_subtract_data pstate goal r1 r2
  | RlEmptyArrayRight r2 ->
    compare_rule_unfold_head_vs_empty_array_right pstate goal r1 r2
  | _ -> herror "compare_rule_unfold_head_vs_others: not expect" pr_rule r2
;;

(*** compare rule unfold view left with others ***)

let compare_rule_unfold_view_left_vs_unfold_head pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_head_vs_unfold_view_left pstate goal r2 r1)
;;

let compare_rule_unfold_view_left_vs_unfold_view_left pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_unfold_view_left_vs_unfold_view_right pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_unfold_view_left_vs_match_data pstate goal r1 r2 =
  try
    if r2.rmd_same_root then raise_prio PrioLow;
    (* default *)
    PrioEqual
  with EPrio p -> p
;;

let compare_rule_unfold_view_left_vs_match_view pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_unfold_view_left_vs_match_array pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_unfold_view_left_vs_subtract_data pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_unfold_view_left_vs_empty_array_right pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_unfold_view_left_vs_others pstate goal r1 r2 =
  match r2 with
  | RlUnfoldHead r2 ->
    compare_rule_unfold_view_left_vs_unfold_head pstate goal r1 r2
  | RlUnfoldViewLeft r2 ->
    compare_rule_unfold_view_left_vs_unfold_view_left pstate goal r1 r2
  | RlUnfoldViewRight r2 ->
    compare_rule_unfold_view_left_vs_unfold_view_right pstate goal r1 r2
  | RlMatchData r2 ->
    compare_rule_unfold_view_left_vs_match_data pstate goal r1 r2
  | RlMatchView r2 ->
    compare_rule_unfold_view_left_vs_match_view pstate goal r1 r2
  | RlMatchArray r2 ->
    compare_rule_unfold_view_left_vs_match_array pstate goal r1 r2
  | RlSubtractData r2 ->
    compare_rule_unfold_view_left_vs_subtract_data pstate goal r1 r2
  | RlEmptyArrayRight r2 ->
    compare_rule_unfold_view_left_vs_empty_array_right pstate goal r1 r2
  | _ ->
    herror "compare_rule_unfold_view_left_vs_others: not expect" pr_rule r2
;;

(*** compare rule unfold view right with others ***)

let compare_rule_unfold_view_right_vs_unfold_head pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_head_vs_unfold_view_right pstate goal r2 r1)
;;

let compare_rule_unfold_view_right_vs_unfold_view_left pstate goal r1 r2 =
  neg_prio
    (compare_rule_unfold_view_left_vs_unfold_view_right pstate goal r2 r1)
;;

let compare_rule_unfold_view_right_vs_unfold_view_right pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_unfold_view_right_vs_match_data pstate goal r1 r2 =
  try
    if r2.rmd_same_root then raise_prio PrioLow;
    (* default *)
    PrioEqual
  with EPrio p -> p
;;

let compare_rule_unfold_view_right_vs_match_view pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_unfold_view_right_vs_match_array pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_unfold_view_right_vs_subtract_data pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_unfold_view_right_vs_empty_array_right pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_unfold_view_right_vs_others pstate goal r1 r2 =
  match r2 with
  | RlUnfoldHead r2 ->
    compare_rule_unfold_view_right_vs_unfold_head pstate goal r1 r2
  | RlUnfoldViewLeft r2 ->
    compare_rule_unfold_view_right_vs_unfold_view_left pstate goal r1 r2
  | RlUnfoldViewRight r2 ->
    compare_rule_unfold_view_right_vs_unfold_view_right pstate goal r1 r2
  | RlMatchData r2 ->
    compare_rule_unfold_view_right_vs_match_data pstate goal r1 r2
  | RlMatchView r2 ->
    compare_rule_unfold_view_right_vs_match_view pstate goal r1 r2
  | RlMatchArray r2 ->
    compare_rule_unfold_view_right_vs_match_array pstate goal r1 r2
  | RlSubtractData r2 ->
    compare_rule_unfold_view_right_vs_subtract_data pstate goal r1 r2
  | RlEmptyArrayRight r2 ->
    compare_rule_unfold_view_right_vs_empty_array_right pstate goal r1 r2
  | _ ->
    herror "compare_rule_unfold_view_right_vs_others: not expect" pr_rule r2
;;

(*** compare rule match data with others ***)

let compare_rule_match_data_vs_unfold_head pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_head_vs_match_data pstate goal r2 r1)
;;

let compare_rule_match_data_vs_unfold_view_left pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_view_left_vs_match_data pstate goal r2 r1)
;;

let compare_rule_match_data_vs_unfold_view_right pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_view_right_vs_match_data pstate goal r2 r1)
;;

let compare_rule_match_data_vs_match_data pstate goal r1 r2 =
  try
    if r1.rmd_same_root && r2.rmd_same_root then raise_prio PrioEqual;
    if r1.rmd_same_root && not r2.rmd_same_root then raise_prio PrioHigh;
    if (not r1.rmd_same_root) && r2.rmd_same_root then raise_prio PrioLow;
    (* default *)
    PrioEqual
  with EPrio p -> p
;;

let compare_rule_match_data_vs_match_view pstate goal r1 r2 =
  try
    if r1.rmd_same_root then raise_prio PrioHigh;
    if r2.rmv_same_args then raise_prio PrioLow;
    (* default *)
    PrioEqual
  with EPrio p -> p
;;

let compare_rule_match_data_vs_match_array pstate goal r1 r2 =
  try
    if r1.rmd_same_root then raise_prio PrioHigh;
    if r2.rma_same_args then raise_prio PrioLow;
    (* default *)
    PrioEqual
  with EPrio p -> p
;;

let compare_rule_match_data_vs_subtract_data pstate goal r1 r2 =
  try
    if r1.rmd_same_root then raise_prio PrioHigh;
    (* default *)
    PrioLow
  with EPrio p -> p
;;

let compare_rule_match_data_vs_empty_array_right pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_match_data_vs_others pstate goal r1 r2 =
  match r2 with
  | RlUnfoldHead r2 -> compare_rule_match_data_vs_unfold_head pstate goal r1 r2
  | RlUnfoldViewLeft r2 ->
    compare_rule_match_data_vs_unfold_view_left pstate goal r1 r2
  | RlUnfoldViewRight r2 ->
    compare_rule_match_data_vs_unfold_view_right pstate goal r1 r2
  | RlMatchData r2 -> compare_rule_match_data_vs_match_data pstate goal r1 r2
  | RlMatchView r2 -> compare_rule_match_data_vs_match_view pstate goal r1 r2
  | RlMatchArray r2 -> compare_rule_match_data_vs_match_array pstate goal r1 r2
  | RlSubtractData r2 ->
    compare_rule_match_data_vs_subtract_data pstate goal r1 r2
  | RlEmptyArrayRight r2 ->
    compare_rule_match_data_vs_empty_array_right pstate goal r1 r2
  | _ -> herror "compare_rule_match_data_vs_others: not expect" pr_rule r2
;;

(*** compare rule match view with others ***)

let compare_rule_match_view_vs_unfold_head pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_head_vs_match_view pstate goal r2 r1)
;;

let compare_rule_match_view_vs_unfold_view_left pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_view_left_vs_match_view pstate goal r2 r1)
;;

let compare_rule_match_view_vs_unfold_view_right pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_view_right_vs_match_view pstate goal r2 r1)
;;

let compare_rule_match_view_vs_match_data pstate goal r1 r2 =
  neg_prio (compare_rule_match_data_vs_match_view pstate goal r2 r1)
;;

let compare_rule_match_view_vs_match_view pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_match_view_vs_match_array pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_match_view_vs_subtract_data pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_match_view_vs_empty_array_right pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_match_view_vs_others pstate goal r1 r2 =
  match r2 with
  | RlUnfoldHead r2 -> compare_rule_match_view_vs_unfold_head pstate goal r1 r2
  | RlUnfoldViewLeft r2 ->
    compare_rule_match_view_vs_unfold_view_left pstate goal r1 r2
  | RlUnfoldViewRight r2 ->
    compare_rule_match_view_vs_unfold_view_right pstate goal r1 r2
  | RlMatchData r2 -> compare_rule_match_view_vs_match_data pstate goal r1 r2
  | RlMatchView r2 -> compare_rule_match_view_vs_match_view pstate goal r1 r2
  | RlMatchArray r2 -> compare_rule_match_view_vs_match_array pstate goal r1 r2
  | RlSubtractData r2 ->
    compare_rule_match_view_vs_subtract_data pstate goal r1 r2
  | RlEmptyArrayRight r2 ->
    compare_rule_match_view_vs_empty_array_right pstate goal r1 r2
  | _ -> herror "compare_rule_match_view_vs_others: not expect" pr_rule r2
;;

(*** compare rule match array with others ***)

let compare_rule_match_array_vs_unfold_head pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_head_vs_match_array pstate goal r2 r1)
;;

let compare_rule_match_array_vs_unfold_view_left pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_view_left_vs_match_array pstate goal r2 r1)
;;

let compare_rule_match_array_vs_unfold_view_right pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_view_right_vs_match_array pstate goal r2 r1)
;;

let compare_rule_match_array_vs_match_data pstate goal r1 r2 =
  neg_prio (compare_rule_match_data_vs_match_array pstate goal r2 r1)
;;

let compare_rule_match_array_vs_match_view pstate goal r1 r2 =
  neg_prio (compare_rule_match_view_vs_match_array pstate goal r2 r1)
;;

let compare_rule_match_array_vs_match_array pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_match_array_vs_subtract_data pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_match_array_vs_empty_array_right pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_match_array_vs_others pstate goal r1 r2 =
  match r2 with
  | RlUnfoldHead r2 ->
    compare_rule_match_array_vs_unfold_head pstate goal r1 r2
  | RlUnfoldViewLeft r2 ->
    compare_rule_match_array_vs_unfold_view_left pstate goal r1 r2
  | RlUnfoldViewRight r2 ->
    compare_rule_match_array_vs_unfold_view_right pstate goal r1 r2
  | RlMatchData r2 -> compare_rule_match_array_vs_match_data pstate goal r1 r2
  | RlMatchView r2 -> compare_rule_match_array_vs_match_view pstate goal r1 r2
  | RlMatchArray r2 ->
    compare_rule_match_array_vs_match_array pstate goal r1 r2
  | RlSubtractData r2 ->
    compare_rule_match_array_vs_subtract_data pstate goal r1 r2
  | RlEmptyArrayRight r2 ->
    compare_rule_match_array_vs_empty_array_right pstate goal r1 r2
  | _ -> herror "compare_rule_match_array_vs_others: not expect" pr_rule r2
;;

(*** compare rule subtract data with others ***)

let compare_rule_subtract_data_vs_unfold_head pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_head_vs_subtract_data pstate goal r2 r1)
;;

let compare_rule_subtract_data_vs_unfold_view_left pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_view_left_vs_subtract_data pstate goal r2 r1)
;;

let compare_rule_subtract_data_vs_unfold_view_right pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_view_right_vs_subtract_data pstate goal r2 r1)
;;

let compare_rule_subtract_data_vs_match_data pstate goal r1 r2 =
  neg_prio (compare_rule_match_data_vs_subtract_data pstate goal r2 r1)
;;

let compare_rule_subtract_data_vs_match_view pstate goal r1 r2 =
  neg_prio (compare_rule_match_view_vs_subtract_data pstate goal r2 r1)
;;

let compare_rule_subtract_data_vs_match_array pstate goal r1 r2 =
  neg_prio (compare_rule_match_array_vs_subtract_data pstate goal r2 r1)
;;

let compare_rule_subtract_data_vs_subtract_data pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_subtract_data_vs_empty_array_right pstate goal r1 r2 =
  try (* default *)
      PrioLow with EPrio p -> p
;;

let compare_rule_subtract_data_vs_others pstate goal r1 r2 =
  match r2 with
  | RlUnfoldHead r2 ->
    compare_rule_subtract_data_vs_unfold_head pstate goal r1 r2
  | RlUnfoldViewLeft r2 ->
    compare_rule_subtract_data_vs_unfold_view_left pstate goal r1 r2
  | RlUnfoldViewRight r2 ->
    compare_rule_subtract_data_vs_unfold_view_right pstate goal r1 r2
  | RlMatchData r2 ->
    compare_rule_subtract_data_vs_match_data pstate goal r1 r2
  | RlMatchView r2 ->
    compare_rule_subtract_data_vs_match_view pstate goal r1 r2
  | RlMatchArray r2 ->
    compare_rule_subtract_data_vs_match_array pstate goal r1 r2
  | RlSubtractData r2 ->
    compare_rule_subtract_data_vs_subtract_data pstate goal r1 r2
  | RlEmptyArrayRight r2 ->
    compare_rule_subtract_data_vs_empty_array_right pstate goal r1 r2
  | _ -> herror "compare_rule_subtract_data_vs_others: not expect" pr_rule r2
;;

(*** compare rule empty array right with others ***)

let compare_rule_empty_array_right_vs_unfold_head pstate goal r1 r2 =
  neg_prio (compare_rule_unfold_head_vs_empty_array_right pstate goal r2 r1)
;;

let compare_rule_empty_array_right_vs_unfold_view_left pstate goal r1 r2 =
  neg_prio
    (compare_rule_unfold_view_left_vs_empty_array_right pstate goal r2 r1)
;;

let compare_rule_empty_array_right_vs_unfold_view_right pstate goal r1 r2 =
  neg_prio
    (compare_rule_unfold_view_right_vs_empty_array_right pstate goal r2 r1)
;;

let compare_rule_empty_array_right_vs_match_data pstate goal r1 r2 =
  neg_prio (compare_rule_match_data_vs_empty_array_right pstate goal r2 r1)
;;

let compare_rule_empty_array_right_vs_match_view pstate goal r1 r2 =
  neg_prio (compare_rule_match_view_vs_empty_array_right pstate goal r2 r1)
;;

let compare_rule_empty_array_right_vs_match_array pstate goal r1 r2 =
  neg_prio (compare_rule_match_array_vs_empty_array_right pstate goal r2 r1)
;;

let compare_rule_empty_array_right_vs_subtract_data pstate goal r1 r2 =
  neg_prio (compare_rule_subtract_data_vs_empty_array_right pstate goal r2 r1)
;;

let compare_rule_empty_array_right_vs_empty_array_right pstate goal r1 r2 =
  try (* default *)
      PrioEqual with EPrio p -> p
;;

let compare_rule_empty_array_right_vs_others pstate goal r1 r2 =
  match r2 with
  | RlUnfoldHead r2 ->
    compare_rule_empty_array_right_vs_unfold_head pstate goal r1 r2
  | RlUnfoldViewLeft r2 ->
    compare_rule_empty_array_right_vs_unfold_view_left pstate goal r1 r2
  | RlUnfoldViewRight r2 ->
    compare_rule_empty_array_right_vs_unfold_view_right pstate goal r1 r2
  | RlMatchData r2 ->
    compare_rule_empty_array_right_vs_match_data pstate goal r1 r2
  | RlMatchView r2 ->
    compare_rule_empty_array_right_vs_match_view pstate goal r1 r2
  | RlMatchArray r2 ->
    compare_rule_empty_array_right_vs_match_array pstate goal r1 r2
  | RlSubtractData r2 ->
    compare_rule_empty_array_right_vs_subtract_data pstate goal r1 r2
  | RlEmptyArrayRight r2 ->
    compare_rule_empty_array_right_vs_empty_array_right pstate goal r1 r2
  | _ ->
    herror "compare_rule_empty_array_right_vs_others: not expect" pr_rule r2
;;

(*** compare all rules ***)

let compare_transformation_rules pstate goal r1 r2 =
  match r1 with
  | RlUnfoldHead r1 -> compare_rule_unfold_head_vs_others pstate goal r1 r2
  | RlUnfoldViewLeft r1 ->
    compare_rule_unfold_view_left_vs_others pstate goal r1 r2
  | RlUnfoldViewRight r1 ->
    compare_rule_unfold_view_right_vs_others pstate goal r1 r2
  | RlMatchData r1 -> compare_rule_match_data_vs_others pstate goal r1 r2
  | RlMatchView r1 -> compare_rule_match_view_vs_others pstate goal r1 r2
  | RlMatchArray r1 -> compare_rule_match_array_vs_others pstate goal r1 r2
  | RlSubtractData r1 -> compare_rule_subtract_data_vs_others pstate goal r1 r2
  | RlEmptyArrayRight r1 ->
    compare_rule_empty_array_right_vs_others pstate goal r1 r2
  | _ -> herror "compare_rule: not expect" pr_rule r1
;;

(*******************************************************************
 ** check useless rules
 *******************************************************************)

let is_rule_apply_early rule =
  match rule with
  | RlMatchData r -> r.rmd_apply_early
  | RlMatchView r -> r.rmv_apply_early
  | RlMatchArray r -> r.rma_apply_early
  | _ -> false
;;

let is_rule_unfold_head_useless pstate goal rules r =
  try
    if List.exists ~f:is_rule_apply_early rules then raise_bool true;
    (* default *)
    false
  with EBool res -> res
;;

let is_rule_unfold_view_left_useless pstate goal rules r =
  try
    if List.exists ~f:is_rule_apply_early rules then raise_bool true;
    (* default *)
    false
  with EBool res -> res
;;

let is_rule_unfold_view_right_useless pstate goal rules r =
  try
    if List.exists ~f:is_rule_apply_early rules then raise_bool true;
    (* default *)
    false
  with EBool res -> res
;;

let is_rule_match_data_useless pstate goal rules r =
  try
    if r.rmd_apply_early then raise_bool false;
    if List.exists ~f:is_rule_apply_early rules then raise_bool true;
    (* default *)
    false
  with EBool res -> res
;;

let is_rule_match_view_useless pstate goal rules r =
  try
    if r.rmv_apply_early then raise_bool false;
    if List.exists ~f:is_rule_apply_early rules then raise_bool true;
    (* default *)
    false
  with EBool res -> res
;;

let is_rule_match_array_useless pstate goal rules r =
  try
    if r.rma_apply_early then raise_bool false;
    if List.exists ~f:is_rule_apply_early rules then raise_bool true;
    (* default *)
    false
  with EBool res -> res
;;

let is_rule_useless pstate goal rules rule =
  match rule with
  | RlUnfoldHead r -> is_rule_unfold_head_useless pstate goal rules r
  | RlUnfoldViewLeft r -> is_rule_unfold_view_left_useless pstate goal rules r
  | RlUnfoldViewRight r ->
    is_rule_unfold_view_right_useless pstate goal rules r
  | RlMatchData r -> is_rule_match_data_useless pstate goal rules r
  | RlMatchView r -> is_rule_match_view_useless pstate goal rules r
  | RlMatchArray r -> is_rule_match_array_useless pstate goal rules r
  | _ -> false
;;
