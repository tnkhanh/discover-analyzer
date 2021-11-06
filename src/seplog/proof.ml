(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Libdiscover
open Sprinter
open Printer
open Debugger
open Slir
open Normalize

(*******************************************************************
 ** data structures
 *******************************************************************)

type view_unfold_case =
  { vuc_view : view_form
  ; vuc_orig_form : formula
  ; vuc_case_id : int
  ; vuc_is_base_case : bool
  ; vuc_new_form : formula
  }

type view_unfold_cases = view_unfold_case list

type data_split =
  { dsp_head : data_form
  ; dsp_rest : formula
  ; dsp_evars : var list
  ; dsp_orig_form : formula
  }

type view_split =
  { vsp_head : view_form
  ; vsp_rest : formula
  ; vsp_evars : var list
  ; vsp_orig_form : formula
  }

type array_split =
  { asp_head : array_form
  ; asp_rest : formula
  ; asp_evars : var list
  ; asp_orig_form : formula
  }

type formula_stats =
  { fst_fvs : var list
  ; fst_is_pure : bool
  ; fst_has_mwand : bool
  ; fst_data_splits : data_split list
  ; fst_view_splits : view_split list
  ; fst_array_splits : array_split list
  }

type entail_core =
  { enc_lhs : formula
  ; enc_rhs : formula
  ; enc_lst : formula_stats
  ; enc_rst : formula_stats
  ; enc_id : int
  }

type entail_cores = entail_core list

(* axiom rules *)

type rule_pure_entails = { rap_entails : entail_cores }
type rule_invalid_entail = { rie_entail : entail_core }

(* normalization rules *)

type rule_false_left = { rfl_entails : entail_cores }
type rule_valid_entails = { rve_entails : entail_cores }
type rule_infer_frame = { rif_entail : entail_core }
type rule_elim_bvar = { reb_entails_eqss : (entail_core * equalities) list }
type rule_equal_left = { reql_entails_eqss : (entail_core * equalities) list }
type rule_exists_left = { rexl_entails : entail_cores }
type rule_exists_right = { rexr_entails : entail_cores }

type rule_reln_left =
  { rrl_rdefn : reln_defn
  ; rrl_entail : entail_core
  }

type rule_reln_right =
  { rrr_rdefn : reln_defn
  ; rrr_entail : entail_core
  }

type rule_wand_inner = { rwi_entails : (entail_core * formula list) list }
type rule_wand_outer = { rwo_entails : (entail_core * formula list) list }
type rule_wand_data = { rwd_entails : (entail_core * formula list) list }
type rule_wand_right = { rwr_entail : entail_core }

type rule_data_to_array_left =
  { rdtal_lhs_data : data_form
  ; rdtal_lhs_rest : formula
  ; rdtal_entail : entail_core
  }

(* transformation rules *)

type rule_unfold_head = { ruh_entail : entail_core }

type rule_unfold_view_left =
  { ruvl_lhs_view : view_form
  ; ruvl_lhs_rest : formula
  ; ruvl_lhs_evars : var list
  ; ruvl_entail : entail_core
  }

type rule_unfold_view_right =
  { ruvr_rhs_view : view_form
  ; ruvr_entail : entail_core
  }

type rule_empty_array_right =
  { rear_rhs_array : array_form
  ; rear_rhs_rest : formula
  ; rear_rhs_evs : vars
  ; rear_empty_form : pure_form
  ; rear_entail : entail_core
  }

type rule_match_data =
  { rmd_lhs_data : data_form
  ; rmd_lhs_rest : formula
  ; rmd_rhs_data : data_form
  ; rmd_rhs_rest : formula
  ; rmd_rhs_evars : var list
  ; rmd_matching_form : pure_form
  ; rmd_same_root : bool
  ; rmd_apply_early : bool
  ; rmd_entail : entail_core
  }

type rule_match_view =
  { rmv_lhs_view : view_form
  ; rmv_lhs_rest : formula
  ; rmv_rhs_view : view_form
  ; rmv_rhs_rest : formula
  ; rmv_rhs_evars : var list
  ; rmv_matching_form : pure_form
  ; rmv_same_args : bool
  ; rmv_apply_early : bool
  ; rmv_entail : entail_core
  }

type rule_match_array =
  { rma_lhs_array : array_form
  ; rma_lhs_rest : formula
  ; rma_rhs_array : array_form
  ; rma_rhs_rest : formula
  ; rma_rhs_evars : var list
  ; rma_matching_form : pure_form
  ; rma_same_args : bool
  ; rma_apply_early : bool
  ; rma_entail : entail_core
  }

type rule_subtract_data =
  { rsd_lhs_array : array_form
  ; rsd_lhs_new_array : array_form
  ; rsd_lhs_rest : formula
  ; rsd_rhs_data : data_form
  ; rsd_rhs_rest : formula
  ; rsd_rhs_evars : var list
  ; rsd_matching_form : pure_form
  ; rsd_entail : entail_core
  }

type rule =
  (* axiom rules *)
  | RlNoEntail
  | RlAllPure of rule_pure_entails
  | RlInvalidEntail of rule_invalid_entail
  (* normalization rules *)
  | RlFalseLeft of rule_false_left
  | RlValidEntails of rule_valid_entails
  | RlInferFrame of rule_infer_frame
  | RlElimBVar of rule_elim_bvar
  | RlEqualLeft of rule_equal_left
  | RlExistsLeft of rule_exists_left
  | RlExistsRight of rule_exists_right
  | RlRelnLeft of rule_reln_left
  | RlRelnRight of rule_reln_right
  | RlWandInner of rule_wand_inner
  | RlWandOuter of rule_wand_outer
  | RlWandData of rule_wand_data
  | RlWandRight of rule_wand_right
  (* | RlDataToArrayLeft of rule_data_to_array_left *)
  (* transformation rules *)
  | RlUnfoldHead of rule_unfold_head (* FIMXE: check if this rule is sound? *)
  | RlUnfoldViewLeft of rule_unfold_view_left
  | RlUnfoldViewRight of rule_unfold_view_right
  | RlEmptyArrayRight of rule_empty_array_right
  | RlMatchData of rule_match_data
  | RlMatchView of rule_match_view
  | RlMatchArray of rule_match_array
  | RlSubtractData of rule_subtract_data

type goal =
  { gl_entail_cores : entail_core list
  ; gl_trace : rule list
  }

type derivation_kind =
  | DrvStatus of bool option
  | DrvSubgoals of goal list

type derivation =
  { drv_goal : goal
  ; drv_kind : derivation_kind
  ; drv_rule : rule
  }

type proof_tree =
  { ptr_goal : goal
  ; ptr_rule : rule option
  ; ptr_sub_trees : proof_tree list
  ; ptr_status : bool option
  ; ptr_frames : formula list (* NOTE: can magicwand maintain only one frame? *)
  }

type prover_state =
  { prs_prog : program
  ; mutable prs_interact : bool
  }

(*******************************************************************
 ** exceptions
 *******************************************************************)

exception ERules of rule list
exception EPtree of proof_tree

let raise_first_rule rs =
  if rs != [] then raise (ERules [ List.hd_exn rs ]) else ()
;;

let raise_rules rs = raise (ERules rs)
let raise_ptree pt = raise (EPtree pt)

(*******************************************************************
 ** priority for proof search
 *******************************************************************)

type priority =
  | PrioHigh
  | PrioEqual
  | PrioLow

let neg_prio p =
  match p with
  | PrioHigh -> PrioLow
  | PrioEqual -> PrioEqual
  | PrioLow -> PrioHigh
;;

let pr_prio p =
  match p with
  | PrioHigh -> "High"
  | PrioEqual -> "Equal"
  | PrioLow -> "Low"
;;

let encode_prio p =
  match p with
  | PrioHigh -> 1
  | PrioEqual -> 0
  | PrioLow -> -1
;;

exception EPrio of priority

let raise_prio prio = raise (EPrio prio)

(*******************************************************************
 ** global vars
 *******************************************************************)

let index_entail = ref 0
let index_entail_core = ref 0

(*******************************************************************
 ** printing
 *******************************************************************)

let pr_enc ?(id = false) enc =
  let res = pr_f enc.enc_lhs ^ " |- " ^ pr_f enc.enc_rhs in
  if (not id) || enc.enc_id < 1
  then "# " ^ res
  else "#" ^ pr_int enc.enc_id ^ ". " ^ res
;;

let pr_enc_id (enc : entail_core) : string = "#" ^ pr_int enc.enc_id

let pr_enc_ids (encs : entail_cores) : string =
  encs |> List.map ~f:pr_enc_id |> String.concat ~sep:", "
;;

(* axioms *)

let pr_rne () = "No Entail"
let pr_rph r = "Pure Entails"
let pr_ric r = "Invalid Entail {" ^ pr_enc_id r.rie_entail ^ "}"

(* normalization *)

let pr_rfl r = "False Left {" ^ pr_enc_ids r.rfl_entails ^ "}"
let pr_rve r = "Valid Entails {" ^ pr_enc_ids r.rve_entails ^ "}"
let pr_rif r = "Infer Frame {" ^ pr_enc_id r.rif_entail ^ "}"

let pr_reb r =
  let encs =
    List.fold_left
      ~f:(fun acc (enc, eqs) ->
        let acc = if String.equal acc "" then acc else acc ^ ", " in
        acc ^ pr_enc_id enc ^ ": " ^ pr_equalities eqs)
      ~init:""
      r.reb_entails_eqss in
  "Elim BVar {" ^ encs ^ "}"
;;

let pr_reql r =
  let encs = r.reql_entails_eqss |> List.unzip |> fst in
  "Equal Left {" ^ pr_enc_ids encs ^ "}"
;;

let pr_rexl r = "Exists Left {" ^ pr_enc_ids r.rexl_entails ^ "}"
let pr_rexr r = "Exists Right {" ^ pr_enc_ids r.rexr_entails ^ "}"

let pr_rrl r =
  "Reln Left {" ^ pr_enc_id r.rrl_entail ^ ": " ^ r.rrl_rdefn.relnd_name ^ "}"
;;

let pr_rrr r =
  "Reln Right {" ^ pr_enc_id r.rrr_entail ^ ": " ^ r.rrr_rdefn.relnd_name ^ "}"
;;

let pr_rwi r =
  let encs =
    List.fold_left
      ~f:(fun acc (enc, fs) ->
        let acc = if String.equal acc "" then acc else acc ^ ", " in
        acc ^ pr_enc_id enc ^ ": " ^ pr_fs fs)
      ~init:""
      r.rwi_entails in
  "Wand Inner {" ^ encs ^ "}"
;;

let pr_rwo r =
  let encs =
    List.fold_left
      ~f:(fun acc (enc, fs) ->
        let acc = if String.equal acc "" then acc else acc ^ ", " in
        acc ^ pr_enc_id enc ^ ": " ^ pr_fs fs)
      ~init:""
      r.rwo_entails in
  "Wand Outer {" ^ encs ^ "}"
;;

let pr_rwd r =
  let encs =
    List.fold_left
      ~f:(fun acc (enc, fs) ->
        let acc = if String.equal acc "" then acc else acc ^ ", " in
        acc ^ pr_enc_id enc ^ ": " ^ pr_fs fs)
      ~init:""
      r.rwd_entails in
  "Wand Data {" ^ encs ^ "}"
;;

let pr_rwr r = "Wand Right {" ^ pr_enc_id r.rwr_entail ^ "}"

(* transformations *)

let pr_ruh r =
  let enc = r.ruh_entail in
  "Unfold Head {" ^ pr_enc_id enc ^ ": " ^ pr_f enc.enc_rhs ^ "}"
;;

let pr_ruvl r =
  "Unfold View Left {"
  ^ pr_enc_id r.ruvl_entail
  ^ ": "
  ^ pr_view_form r.ruvl_lhs_view
  ^ "}"
;;

let pr_ruvr r =
  "Unfold View Right {"
  ^ pr_enc_id r.ruvr_entail
  ^ ": "
  ^ pr_view_form r.ruvr_rhs_view
  ^ "}"
;;

let pr_rear r =
  "Unfold Array Right {"
  ^ pr_enc_id r.rear_entail
  ^ ": "
  ^ pr_array_form r.rear_rhs_array
  ^ "}"
;;

let pr_rmd r =
  "Match Data {"
  ^ pr_enc_id r.rmd_entail
  ^ ": "
  ^ pr_df r.rmd_lhs_data
  ^ " vs. "
  ^ pr_df r.rmd_rhs_data
  ^ ", early: "
  ^ pr_bool r.rmd_apply_early
  ^ "}"
;;

let pr_rmv r =
  "Match View {"
  ^ pr_enc_id r.rmv_entail
  ^ ": "
  ^ pr_vf r.rmv_lhs_view
  ^ " vs. "
  ^ pr_vf r.rmv_rhs_view
  ^ ", early: "
  ^ pr_bool r.rmv_apply_early
  ^ "}"
;;

let pr_rma r =
  "Match Array {"
  ^ pr_enc_id r.rma_entail
  ^ ": "
  ^ pr_af r.rma_lhs_array
  ^ " vs. "
  ^ pr_af r.rma_rhs_array
  ^ ", early: "
  ^ pr_bool r.rma_apply_early
  ^ "}"
;;

let pr_rsd r =
  "Subtract Data {"
  ^ pr_enc_id r.rsd_entail
  ^ ": "
  ^ pr_af r.rsd_lhs_array
  ^ " vs. "
  ^ pr_df r.rsd_rhs_data
;;

let pr_rule rule =
  match rule with
  (* axioms *)
  | RlNoEntail -> pr_rne ()
  | RlAllPure r -> pr_rph r
  | RlInvalidEntail r -> pr_ric r
  (* normalizations *)
  | RlFalseLeft r -> pr_rfl r
  | RlValidEntails r -> pr_rve r
  | RlInferFrame r -> pr_rif r
  | RlElimBVar r -> pr_reb r
  | RlEqualLeft r -> pr_reql r
  | RlExistsLeft r -> pr_rexl r
  | RlExistsRight r -> pr_rexr r
  | RlRelnLeft r -> pr_rrl r
  | RlRelnRight r -> pr_rrr r
  | RlWandInner r -> pr_rwi r
  | RlWandOuter r -> pr_rwo r
  | RlWandData r -> pr_rwd r
  | RlWandRight r -> pr_rwr r
  (* | RlDataToArrayLeft r -> pr_rdtal r *)
  (* transformations  *)
  | RlUnfoldHead r -> pr_ruh r
  | RlUnfoldViewLeft r -> pr_ruvl r
  | RlUnfoldViewRight r -> pr_ruvr r
  | RlEmptyArrayRight r -> pr_rear r
  | RlMatchData r -> pr_rmd r
  | RlMatchView r -> pr_rmv r
  | RlMatchArray r -> pr_rma r
  | RlSubtractData r -> pr_rsd r
;;

let pr_rules rules =
  let res =
    List.fold_left ~f:(fun acc r -> acc ^ "\n  - " ^ pr_rule r) ~init:"" rules
  in
  "[" ^ res ^ "]"
;;

let pr_enc_id (enc : entail_core) : string = "#" ^ pr_int enc.enc_id

let pr_goal goal =
  let encs =
    List.fold_left
      ~f:(fun acc enc ->
        acc
        ^ "   #"
        ^ pr_int enc.enc_id
        ^ ". "
        ^ pr_f enc.enc_lhs
        ^ " |- "
        ^ pr_f enc.enc_rhs
        ^ "\n")
      ~init:""
      goal.gl_entail_cores in
  let traces =
    if List.is_empty goal.gl_trace
    then "[]"
    else
      goal.gl_trace |> List.map ~f:pr_rule |> String.concat ~sep:"\n     ==> "
  in
  " @ Entails:\n" ^ encs ^ " @ Trace: " ^ traces
;;

(*******************************************************************
 ** constructors
 *******************************************************************)

(* axiom rules *)

let mk_rule_no_entail () = RlNoEntail
let mk_rule_all_pure encs = RlAllPure { rap_entails = encs }
let mk_rule_invalid_entail enc = RlInvalidEntail { rie_entail = enc }

(* normalization rules *)

let mk_rule_false_left encs = RlFalseLeft { rfl_entails = encs }
let mk_rule_valid_entails encs = RlValidEntails { rve_entails = encs }
let mk_rule_infer_frame enc = RlInferFrame { rif_entail = enc }
let mk_rule_elim_bvar ents_eqss = RlElimBVar { reb_entails_eqss = ents_eqss }
let mk_rule_equal_left ents_eqss = RlEqualLeft { reql_entails_eqss = ents_eqss }
let mk_rule_exists_left encs = RlExistsLeft { rexl_entails = encs }
let mk_rule_exists_right encs = RlExistsRight { rexr_entails = encs }
let mk_rule_reln_left rd enc = RlRelnLeft { rrl_rdefn = rd; rrl_entail = enc }
let mk_rule_reln_right rd enc = RlRelnRight { rrr_rdefn = rd; rrr_entail = enc }
let mk_rule_wand_inner encs = RlWandInner { rwi_entails = encs }
let mk_rule_wand_outer encs = RlWandOuter { rwo_entails = encs }
let mk_rule_wand_data encs = RlWandData { rwd_entails = encs }
let mk_rule_wand_right enc = RlWandRight { rwr_entail = enc }

(* let mk_rule_data_to_array_left df frest enc =
 *   RlDataToArrayLeft {
 *     rdtal_lhs_data = df;
 *     rdtal_lhs_rest = frest;
 *     rdtal_entail = enc;
 *   } *)

(* transformation rules *)

let mk_rule_unfold_head enc = RlUnfoldHead { ruh_entail = enc }

let mk_rule_unfold_view_left vf frest qvars enc =
  RlUnfoldViewLeft
    { ruvl_lhs_view = vf
    ; ruvl_lhs_rest = frest
    ; ruvl_lhs_evars = qvars
    ; ruvl_entail = enc
    }
;;

let mk_rule_unfold_view_right vf enc =
  RlUnfoldViewRight { ruvr_rhs_view = vf; ruvr_entail = enc }
;;

let mk_matching_forms args1 args2 =
  let equal_args =
    List.fold2_exn
      ~f:(fun acc x y -> if equal_exp x y then acc else acc @ [ mk_eq x y ])
      ~init:[]
      args1
      args2 in
  mk_pconj equal_args
;;

let mk_rule_match_data enc ldsp rdsp =
  let ldf, rdf = ldsp.dsp_head, rdsp.dsp_head in
  let matching_form =
    let largs = ldf.data_root :: ldf.data_args in
    let rargs = rdf.data_root :: rdf.data_args in
    mk_matching_forms largs rargs in
  let has_same_root = equal_exp ldf.data_root rdf.data_root in
  RlMatchData
    { rmd_lhs_data = ldsp.dsp_head
    ; rmd_lhs_rest = ldsp.dsp_rest
    ; rmd_rhs_data = rdsp.dsp_head
    ; rmd_rhs_rest = rdsp.dsp_rest
    ; rmd_rhs_evars = rdsp.dsp_evars
    ; rmd_same_root = has_same_root
    ; rmd_matching_form = matching_form
    ; rmd_apply_early = has_same_root
    ; rmd_entail = enc
    }
;;

let mk_rule_match_view enc lvsp rvsp =
  let lvf, rvf = lvsp.vsp_head, rvsp.vsp_head in
  let matching_form = mk_matching_forms lvf.view_args rvf.view_args in
  let has_same_args =
    List.for_all2_exn ~f:equal_exp lvf.view_args rvf.view_args in
  RlMatchView
    { rmv_lhs_view = lvsp.vsp_head
    ; rmv_lhs_rest = lvsp.vsp_rest
    ; rmv_rhs_view = rvsp.vsp_head
    ; rmv_rhs_rest = rvsp.vsp_rest
    ; rmv_rhs_evars = rvsp.vsp_evars
    ; rmv_matching_form = matching_form
    ; rmv_same_args = has_same_args
    ; rmv_apply_early = has_same_args
    ; rmv_entail = enc
    }
;;

let mk_rule_match_array enc lasp rasp =
  let laf, raf = lasp.asp_head, rasp.asp_head in
  let matching_form =
    let largs = [ laf.array_root; laf.array_size ] in
    let rargs = [ raf.array_root; raf.array_size ] in
    mk_matching_forms largs rargs in
  let has_same_args =
    equal_exp laf.array_root raf.array_root
    && equal_exp laf.array_size raf.array_size in
  RlMatchArray
    { rma_lhs_array = lasp.asp_head
    ; rma_lhs_rest = lasp.asp_rest
    ; rma_rhs_array = rasp.asp_head
    ; rma_rhs_rest = rasp.asp_rest
    ; rma_rhs_evars = rasp.asp_evars
    ; rma_matching_form = matching_form
    ; rma_same_args = has_same_args
    ; rma_apply_early = has_same_args
    ; rma_entail = enc
    }
;;

let mk_rule_subtract_data enc lasp rdsp =
  let laf, rdf = lasp.asp_head, rdsp.dsp_head in
  let matched_dfs, other_update =
    List.partition_tf
      ~f:(fun d -> equal_exp d.data_root rdf.data_root)
      laf.array_update in
  let matched_df =
    match matched_dfs with
    | [] ->
      let nargs =
        List.map ~f:(fun e -> fresh_exp_var (typ_of_exp e)) rdf.data_args in
      { rdf with data_args = nargs }
    | [ d ] -> d
    | _ -> error "mk_rule_subtract_data: overlapped updating data" in
  let new_array =
    let nlaf = { laf with array_update = other_update } in
    subtract_array_form nlaf [ matched_df ] in
  let matching_form =
    let largs = matched_df.data_root :: matched_df.data_args in
    let rargs = rdf.data_root :: rdf.data_args in
    mk_matching_forms largs rargs in
  RlSubtractData
    { rsd_lhs_array = lasp.asp_head
    ; rsd_lhs_new_array = new_array
    ; rsd_lhs_rest = lasp.asp_rest
    ; rsd_rhs_data = rdsp.dsp_head
    ; rsd_rhs_rest = rdsp.dsp_rest
    ; rsd_rhs_evars = rdsp.dsp_evars
    ; rsd_matching_form = matching_form
    ; rsd_entail = enc
    }
;;

let mk_rule_empty_array_right enc af rest evs =
  let empty_form = mk_eq af.array_size (mk_exp_int 0) in
  RlEmptyArrayRight
    { rear_rhs_array = af
    ; rear_rhs_rest = rest
    ; rear_rhs_evs = evs
    ; rear_empty_form = empty_form
    ; rear_entail = enc
    }
;;

(* goal *)

let fresh_entail_core_id () =
  let _ = index_entail_core := !index_entail_core + 1 in
  !index_entail_core
;;

let mk_view_unfold_cases prog orig_form vf frest evars : view_unfold_cases =
  let vdcs = unfold_view_form prog.prog_view_defns vf in
  List.map
    ~f:(fun vdc ->
      let nf = mk_f_star [ vdc.vdc_form; frest ] in
      let nf = simplify_f (mk_f_exists evars nf) in
      { vuc_view = vf
      ; vuc_orig_form = orig_form
      ; vuc_case_id = vdc.vdc_id
      ; vuc_is_base_case = vdc.vdc_is_base_case
      ; vuc_new_form = nf
      })
    vdcs
;;

let mk_data_split orig_form head rest evars =
  { dsp_head = head
  ; dsp_rest = rest
  ; dsp_evars = evars
  ; dsp_orig_form = orig_form
  }
;;

let mk_view_split orig_form head rest evars =
  { vsp_head = head
  ; vsp_rest = rest
  ; vsp_evars = evars
  ; vsp_orig_form = orig_form
  }
;;

let mk_array_split orig_form head rest evars =
  { asp_head = head
  ; asp_rest = rest
  ; asp_evars = evars
  ; asp_orig_form = orig_form
  }
;;

let split_one_data_form (f : formula) : data_split list =
  let rec extract acc used_fs new_fs : (data_form * formula list) list =
    match new_fs with
    | [] -> acc
    | (Data df as f) :: new_fs ->
      let acc = acc @ [ df, used_fs @ new_fs ] in
      extract acc (used_fs @ [ f ]) new_fs
    | f :: new_fs -> extract acc (used_fs @ [ f ]) new_fs in
  let rec split_f f : (data_form * formula * vars) list =
    match f with
    | Pure _ | Emp | View _ | Iter _ | Array _ | Wand _ | Septract _ -> []
    | Data df -> [ df, Emp, [] ]
    | Star fs ->
      fs |> extract [] [] |> List.map ~f:(fun (df, fs) -> df, mk_f_star fs, [])
    | Exists (vs, f) ->
      f |> split_f |> List.map ~f:(fun (df, nf, nvs) -> df, nf, nvs @ vs) in
  List.map ~f:(fun (df, nf, qvs) -> mk_data_split f df nf qvs) (split_f f)
;;

let split_one_view_form (f : formula) : view_split list =
  let rec extract acc used_fs new_fs : (view_form * formula list) list =
    match new_fs with
    | [] -> acc
    | (View vf as f) :: new_fs ->
      let acc = acc @ [ vf, used_fs @ new_fs ] in
      extract acc (used_fs @ [ f ]) new_fs
    | f :: new_fs -> extract acc (used_fs @ [ f ]) new_fs in
  let rec split_f f : (view_form * formula * vars) list =
    match f with
    | Pure _ | Emp | Data _ | Iter _ | Array _ -> []
    | Wand _ | Septract _ -> []
    | View vf -> [ vf, Emp, [] ]
    | Star fs ->
      fs |> extract [] [] |> List.map ~f:(fun (vf, fs) -> vf, mk_f_star fs, [])
    | Exists (vs, f) ->
      f |> split_f |> List.map ~f:(fun (vf, nf, nvs) -> vf, nf, nvs @ vs) in
  List.map ~f:(fun (vf, nf, qvs) -> mk_view_split f vf nf qvs) (split_f f)
;;

let split_one_array_form (f : formula) : array_split list =
  let rec extract acc used_fs new_fs : (array_form * formula list) list =
    match new_fs with
    | [] -> acc
    | (Array af as f) :: new_fs ->
      let acc = acc @ [ af, used_fs @ new_fs ] in
      extract acc (used_fs @ [ f ]) new_fs
    | f :: new_fs -> extract acc (used_fs @ [ f ]) new_fs in
  let rec split_f f : (array_form * formula * vars) list =
    match f with
    | Pure _ | Emp | Data _ | View _ | Iter _ -> []
    | Wand _ | Septract _ -> []
    | Array af -> [ af, Emp, [] ]
    | Star fs ->
      fs |> extract [] [] |> List.map ~f:(fun (af, fs) -> af, mk_f_star fs, [])
    | Exists (vs, f) ->
      f |> split_f |> List.map ~f:(fun (af, nf, nvs) -> af, nf, nvs @ vs) in
  List.map ~f:(fun (af, nf, qvs) -> mk_array_split f af nf qvs) (split_f f)
;;

let mk_formula_stats f =
  { fst_fvs = fv_f f
  ; fst_is_pure = is_f_pure f
  ; fst_has_mwand = has_f_wand f
  ; fst_data_splits = split_one_data_form f
  ; fst_view_splits = split_one_view_form f
  ; fst_array_splits = split_one_array_form f
  }
;;

let mk_entail_core ?(id = 0) lhs rhs =
  let enc_id = if id < 1 then fresh_entail_core_id () else id in
  { enc_lhs = lhs
  ; enc_rhs = rhs
  ; enc_lst = mk_formula_stats lhs
  ; enc_rst = mk_formula_stats rhs
  ; enc_id
  }
;;

let mk_entail_core_from_entailment ent =
  mk_entail_core ent.ent_lhs ent.ent_rhs ~id:ent.ent_id
;;

let mk_goal (ents : entailment list) : goal =
  let encs = List.map ~f:(fun ent -> mk_entail_core_from_entailment ent) ents in
  { gl_entail_cores = encs; gl_trace = [] }
;;

let update_goal_entcores goal encs : goal =
  let encs =
    List.map
      ~f:(fun enc ->
        try List.find_exn ~f:(fun e -> enc.enc_id = e.enc_id) encs with
        | _ -> enc)
      goal.gl_entail_cores in
  { goal with gl_entail_cores = encs }
;;

let remove_goal_entcores goal encs : goal =
  let nents =
    List.fold_left
      ~f:(fun acc enc ->
        if List.exists ~f:(fun e -> enc.enc_id = e.enc_id) encs
        then acc
        else acc @ [ enc ])
      ~init:[]
      goal.gl_entail_cores in
  { goal with gl_entail_cores = nents }
;;

let mk_derivation_subgoals goal sub_goals rule =
  let sub_goals =
    List.map ~f:(fun g -> { g with gl_trace = g.gl_trace @ [ rule ] }) sub_goals
  in
  { drv_goal = goal; drv_kind = DrvSubgoals sub_goals; drv_rule = rule }
;;

let mk_derivation_valid goal rule =
  { drv_goal = goal; drv_kind = DrvStatus (Some true); drv_rule = rule }
;;

let mk_derivation_invalid goal rule =
  { drv_goal = goal; drv_kind = DrvStatus (Some false); drv_rule = rule }
;;

let mk_derivation_unknown goal rule =
  { drv_goal = goal; drv_kind = DrvStatus None; drv_rule = rule }
;;

let mk_proof_tree ?(frames = []) goal rule subtrees status =
  { ptr_goal = goal
  ; ptr_rule = rule
  ; ptr_sub_trees = subtrees
  ; ptr_status = status
  ; ptr_frames = frames
  }
;;

let mk_proof_tree_valid goal rule subtrees : proof_tree =
  let frames =
    List.fold_left ~f:(fun acc pt -> acc @ pt.ptr_frames) ~init:[] subtrees
  in
  let frames =
    match rule with
    | RlInferFrame r -> [ r.rif_entail.enc_lhs ] @ frames
    | RlMatchData r ->
      List.map
        ~f:(fun f -> mk_f_star_with f ~pfs:[ r.rmd_matching_form ])
        frames
    | RlMatchView r ->
      List.map
        ~f:(fun f -> mk_f_star_with f ~pfs:[ r.rmv_matching_form ])
        frames
    | RlMatchArray r ->
      List.map
        ~f:(fun f -> mk_f_star_with f ~pfs:[ r.rma_matching_form ])
        frames
    | RlEmptyArrayRight r ->
      List.map ~f:(fun f -> mk_f_star_with f ~pfs:[ r.rear_empty_form ]) frames
    | _ -> frames in
  mk_proof_tree goal (Some rule) subtrees (Some true) ~frames
;;

let mk_proof_tree_invalid goal rule subtrees : proof_tree =
  mk_proof_tree goal (Some rule) subtrees (Some false)
;;

let mk_proof_tree_unknown goal : proof_tree = mk_proof_tree goal None [] None

let mk_prover_state prog : prover_state =
  { prs_prog = prog; prs_interact = !mode_interactive_prover }
;;

(*******************************************************************
 ** querries
 *******************************************************************)

let get_entail_of_entcore enc : entailment =
  { ent_lhs = enc.enc_lhs; ent_rhs = enc.enc_rhs; ent_id = enc.enc_id }
;;

let get_entcore_of_entail ent : entail_core =
  mk_entail_core ent.ent_lhs ent.ent_rhs ~id:ent.ent_id
;;

let get_entailments_of_goal goal : entailments =
  List.map ~f:get_entail_of_entcore goal.gl_entail_cores
;;

let update_entail_core ?(lhs = []) ?(rhs = []) enc =
  let enc =
    match lhs with
    | [] -> enc
    | [ lhs ] -> { enc with enc_lhs = lhs; enc_lst = mk_formula_stats lhs }
    | _ -> herror "update_entail_core: expect 1 lhs but found: " pr_fs lhs in
  let enc =
    match rhs with
    | [] -> enc
    | [ rhs ] -> { enc with enc_rhs = rhs; enc_rst = mk_formula_stats rhs }
    | _ -> herror "update_entail_core: expect 1 rhs but found: " pr_fs rhs in
  enc
;;
