(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Llir
open Int_interval
module DF = Dataflow
module LL = Llvm
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LV = LL.ValueKind
module LA = List.Assoc
module BG = Bug
module SP = Set.Poly
module MP = Map.Poly

(*******************************************************************
 ** Core data transfer modules
 *******************************************************************)

module IntervalData = struct
  open Int_interval

  type t = (expr, interval) MP.t
end

module RangeUtil = struct
  include IntervalData

  let get_interval (e : expr) (d : t) : interval =
    match e with
    | Int64 i -> interval_of_bound (Int64 i)
    | _ ->
      (match MP.find d e with
      | Some i -> i
      | None ->
        let etyp = type_of_expr e in
        if is_type_integer etyp
        then (
          let bw = LL.integer_bitwidth etyp in
          compute_int_max_interval bw)
        else least_interval)
  ;;

  let replace_interval (e : expr) (i : interval) (d : t) : t =
    let nd =
      match MP.find d e with
      | None -> d
      | Some a -> MP.remove d e in
    MP.add_exn ~key:e ~data:i nd
  ;;
end

module RangeTransfer : DF.ForwardDataTransfer with type t = IntervalData.t =
struct
  include IntervalData
  include RangeUtil
  include DF.MakeDefaultEnv (IntervalData)

  let analysis = DfaRange

  (*******************************************************************
   ** Handling abstract data
   *******************************************************************)

  (* constants *)

  let least_data : t = MP.empty

  (* printers *)

  let pr_data (d : t) =
    let expr_interval_lst = MP.to_alist ~key_order:`Decreasing d in
    pr_list_square
      ~f:(fun (e, i) -> pr_expr e ^ ": " ^ pr_interval i)
      expr_interval_lst
  ;;

  let pr_data_checksum = pr_data

  (* comparison*)

  let equal_data (d1 : t) (d2 : t) : bool = MP.equal equal_interval d1 d2

  let lequal_data (d1 : t) (d2 : t) : bool =
    let diffs = MP.symmetric_diff d1 d2 ~data_equal:lequal_interval in
    Sequence.for_all
      ~f:(fun diff ->
        match snd diff with
        | `Left _ -> false
        | `Unequal _ -> false
        | `Right _ -> true)
      diffs
  ;;

  (*   MP.for_alli *)
  (*     ~f:(fun ~key:e1 ~data:i1 -> *)
  (*       MP.existsi *)
  (*         ~f:(fun ~key:e2 ~data:i2 -> *)
  (*           equal_expr e1 e2 && lequal_interval i1 i2) *)
  (*         d2) *)
  (*     d1 *)
  (* ;; *)

  let copy_data d = d

  (* substitution *)

  let subst_data
      ?(sstv : substv = [])
      ?(sstve : substve = [])
      ?(sste : subste = [])
      (d : t)
      : t
    =
    MP.fold
      ~f:(fun ~key:e ~data:i acc ->
        let ne = subst_expr ~sstv ~sstve ~sste e in
        match MP.add acc ~key:ne ~data:i with
        | `Ok res -> res
        | `Duplicate ->
          let _ = hwarning "Range.subst_data: duplicate expr: " pr_expr ne in
          acc)
      ~init:MP.empty d
  ;;

  let merge_data ?(widen = false) (d1 : t) (d2 : t) : t =
    MP.merge
      ~f:(fun ~key:e i ->
        match i with
        | `Both (i1, i2) -> Some (combine_interval i1 i2)
        | `Left i1 -> Some i1
        | `Right i2 -> Some i2)
      d1 d2
  ;;

  (* FIXME: fix this later *)
  let join_data (d1 : t) (d2 : t) : t = merge_data d1 d2

  let clean_irrelevant_info_from_data penv func (d : t) : t =
    MP.filteri
      ~f:(fun ~key:e ~data:i ->
        let vs = collect_llvalue_of_expr e in
        not (List.exists ~f:is_local_llvalue vs))
      d
  ;;

  let clean_info_of_vars (input : t) (vs : values) : t =
    (* TODO: implement later *)
    input
  ;;

  let extract_data_from_predicate ~(widen : bool) (p : predicate) (d : t) : t =
    match p with
    | PBool _ -> MP.empty
    | PIcmp (cmp, lhs, rhs) ->
      let lhs, rhs = expr_of_value lhs, expr_of_value rhs in
      if is_expr_const lhs && not (is_expr_const rhs)
      then (
        let b =
          match extract_constant_bound lhs with
          | Some b -> b
          | None -> herror "extract_const_bound_lhs: not found:" pr_expr lhs
        in
        match cmp with
        | LC.Eq -> MP.of_alist_exn [ rhs, interval_of_bound b ]
        | LC.Ne -> MP.empty (* TODO: can be improved here to be more precise *)
        | LC.Ugt | LC.Sgt ->
          MP.of_alist_exn [ rhs, mk_interval_range ~ui:false NInf b ]
        | LC.Uge | LC.Sge ->
          MP.of_alist_exn [ rhs, mk_interval_range ~ui:true NInf b ]
        | LC.Ult | LC.Slt ->
          MP.of_alist_exn [ rhs, mk_interval_range ~li:false b PInf ]
        | LC.Ule | LC.Sle ->
          MP.of_alist_exn [ rhs, mk_interval_range ~li:true b PInf ])
      else if (not (is_expr_const lhs)) && is_expr_const rhs
      then (
        let b =
          match extract_constant_bound rhs with
          | Some b -> b
          | None -> herror "extract_const_bound_rhs: not found:" pr_expr rhs
        in
        match cmp with
        | LC.Eq -> MP.of_alist_exn [ lhs, interval_of_bound b ]
        | LC.Ne -> MP.empty (* TODO: can be improved here to be more precise *)
        | LC.Ugt | LC.Sgt ->
          MP.of_alist_exn [ lhs, mk_interval_range ~li:false b PInf ]
        | LC.Uge | LC.Sge ->
          MP.of_alist_exn [ lhs, mk_interval_range ~li:true b PInf ]
        | LC.Ult | LC.Slt ->
          MP.of_alist_exn [ lhs, mk_interval_range ~ui:false NInf b ]
        | LC.Ule | LC.Sle ->
          MP.of_alist_exn [ lhs, mk_interval_range ~ui:true NInf b ])
      else (
        let ilhs, irhs = get_interval lhs d, get_interval rhs d in
        match cmp with
        | LC.Eq -> MP.of_alist_exn [ lhs, irhs; rhs, ilhs ]
        | LC.Ne -> MP.empty (* TODO: can be improved here to be more precise *)
        | LC.Ugt | LC.Sgt ->
          let nilhs =
            match irhs with
            | Range r ->
              if widen
              then mk_interval_range ~li:false r.range_lb PInf
              else mk_interval_range ~li:false r.range_ub PInf
            | Bottom -> Bottom in
          let nirhs =
            match ilhs with
            | Range r ->
              if widen
              then mk_interval_range ~ui:false NInf r.range_ub
              else mk_interval_range ~ui:false NInf r.range_lb
            | Bottom -> Bottom in
          MP.of_alist_exn [ lhs, nilhs; rhs, nirhs ]
        | LC.Uge | LC.Sge ->
          let nilhs =
            match irhs with
            | Range r ->
              if widen
              then mk_interval_range ~li:true r.range_lb PInf
              else mk_interval_range ~li:true r.range_ub PInf
            | Bottom -> Bottom in
          let nirhs =
            match ilhs with
            | Range r ->
              if widen
              then mk_interval_range ~ui:true NInf r.range_ub
              else mk_interval_range ~ui:true NInf r.range_lb
            | Bottom -> Bottom in
          MP.of_alist_exn [ lhs, nilhs; rhs, nirhs ]
        | LC.Ult | LC.Slt ->
          let nilhs =
            match irhs with
            | Range r ->
              if widen
              then mk_interval_range ~ui:false NInf r.range_ub
              else mk_interval_range ~ui:false NInf r.range_lb
            | Bottom -> Bottom in
          let nirhs =
            match ilhs with
            | Range r ->
              if widen
              then mk_interval_range ~li:false r.range_lb PInf
              else mk_interval_range ~li:false r.range_ub PInf
            | Bottom -> Bottom in
          MP.of_alist_exn [ lhs, nilhs; rhs, nirhs ]
        | LC.Ule | LC.Sle ->
          let nilhs =
            match irhs with
            | Range r ->
              if widen
              then mk_interval_range ~ui:true NInf r.range_ub
              else mk_interval_range ~ui:true NInf r.range_lb
            | Bottom -> Bottom in
          let nirhs =
            match ilhs with
            | Range r ->
              if widen
              then mk_interval_range ~li:true r.range_lb PInf
              else mk_interval_range ~li:true r.range_ub PInf
            | Bottom -> Bottom in
          MP.of_alist_exn [ lhs, nilhs; rhs, nirhs ])
    | PFcmp _ -> MP.empty
    | PNeg _ | PConj _ | PDisj _ ->
      herror "extract_data_from_predicate: need to handle: " pr_predicate p
  ;;

  let is_data_satisfied_predicate (d : t) (p : predicate) : bool =
    let pd = extract_data_from_predicate ~widen:false p d in
    MP.for_alli
      ~f:(fun ~key:v ~data:vip ->
        let vid = get_interval v d in
        lequal_interval vid vip)
      pd
  ;;

  let refine_data_by_predicate ?(widen = false) (d : t) (p : predicate) : t =
    let pcond_data = extract_data_from_predicate ~widen p d in
    MP.merge
      ~f:(fun ~key:e i ->
        match i with
        | `Both (id, ip) -> Some (intersect_interval id ip)
        | `Left id -> Some id
        | `Right _ -> None)
      d pcond_data
  ;;

  (*******************************************************************
   ** Core analysis functions
   *******************************************************************)

  let need_widening func : bool = true

  let refine_widening func instr widen : bool =
    let users = get_users (llvalue_of_instr instr) in
    let has_int_cmp_relation =
      List.exists
        ~f:(fun u ->
          match LL.classify_value u with
          | LV.Instruction LO.ICmp -> false
          | _ -> true)
        users in
    if not has_int_cmp_relation then widen else true
  ;;

  let prepare_entry_func_input (penv : prog_env) func (input : t) : t =
    let _ = debug "PREPARE_ENTRY_FUNC_INPUT" in
    let params = func_params func in
    List.fold_left
      ~f:(fun acc param ->
        let _ = hdebug "param: " pr_param param in
        let expr = mk_expr_var (llvalue_of_param param) in
        if is_type_integer (type_of_expr expr)
        then (
          let itv = get_interval expr acc in
          replace_interval expr itv acc)
        else acc)
      ~init:input params
  ;;

  let prepare_callee_input
      (penv : prog_env)
      instr
      func
      (args : values)
      (input : t)
      : t
    =
    List.fold_left
      ~f:(fun acc arg ->
        let earg = mk_expr_var arg in
        let itv = get_interval earg acc in
        replace_interval earg itv acc)
      ~init:input args
  ;;

  let compute_callee_output_exns penv instr callee args input fsum : t * exns =
    input, []
  ;;

  let prepare_thrown_exception_data (penv : prog_env) exn_ptr tinfo (input : t)
      : t
    =
    input
  ;;

  let compute_catch_exception_data (penv : prog_env) instr ptr (input : t) exn
      : t
    =
    input
  ;;

  let analyze_global (g : global) (input : t) : t =
    (* TODO: default behavior, implement later if necessary *)
    input
  ;;

  let analyze_instr
      ?(widen = false)
      (penv : prog_env)
      (fenv : func_env)
      (ins : instr)
      (input : t)
      : t
    =
    let func = func_of_instr ins in
    let expr = expr_of_instr ins in
    match instr_opcode ins with
    | LO.Unreachable -> least_data
    | LO.Add ->
      let opr0, opr1 = operand_expr ins 0, operand_expr ins 1 in
      let itv0, itv1 = get_interval opr0 input, get_interval opr1 input in
      let itv = add_interval itv0 itv1 in
      replace_interval expr itv input
    | LO.Sub ->
      let opr0, opr1 = operand_expr ins 0, operand_expr ins 1 in
      let itv0, itv1 = get_interval opr0 input, get_interval opr1 input in
      let itv = sub_interval itv0 itv1 in
      replace_interval expr itv input
    | LO.Mul ->
      let opr0, opr1 = operand_expr ins 0, operand_expr ins 1 in
      let itv0, itv1 = get_interval opr0 input, get_interval opr1 input in
      let itv = mult_interval itv0 itv1 in
      replace_interval expr itv input
    | LO.UDiv ->
      let opr0, opr1 = operand_expr ins 0, operand_expr ins 1 in
      let itv0, itv1 = get_interval opr0 input, get_interval opr1 input in
      let itv = udiv_interval itv0 itv1 in
      replace_interval expr itv input
    | LO.SDiv ->
      let opr0, opr1 = operand_expr ins 0, operand_expr ins 1 in
      let itv0, itv1 = get_interval opr0 input, get_interval opr1 input in
      let itv = sdiv_interval itv0 itv1 in
      replace_interval expr itv input
    | LO.PHI ->
      let opr0 = operand_expr ins 0 in
      let itv = ref (get_interval opr0 input) in
      let _ = hdebug "Before refine widening: " pr_bool widen in
      let widen = refine_widening func ins widen in
      let _ = hdebug "After refine widening: " pr_bool widen in
      for i = 1 to num_operands ins - 1 do
        let opri = operand_expr ins i in
        let itvi = get_interval opri input in
        (* TODO: need to refine widening here *)
        itv := combine_interval ~widen !itv itvi
      done;
      replace_interval (expr_of_instr ins) !itv input
    | LO.Ret ->
      let expr = mk_expr_func_result func in
      let opr0 = operand_expr ins 0 in
      let itv = get_interval opr0 input in
      replace_interval expr itv input
    | LO.Load ->
      let dst = dst_of_instr_load ins in
      let dst_typ = LL.type_of dst in
      if is_type_integer dst_typ
      then (
        let src = src_of_instr_load ins in
        let src_dref = mk_expr_deref src in
        let src_dref_itv = get_interval src_dref input in
        let dst_expr = mk_expr_var dst in
        replace_interval dst_expr src_dref_itv input)
      else input
    | LO.Store ->
      let src = src_of_instr_store ins in
      let src_typ = LL.type_of src in
      if is_type_integer src_typ
      then (
        let src_itv = get_interval (expr_of_value src) input in
        let dst = dst_of_instr_store ins in
        let dst_dref = mk_expr_deref dst in
        replace_interval dst_dref src_itv input)
      else input
    | LO.ZExt ->
      let esrc = expr_of_value (src_of_instr_zext ins) in
      let itv = get_interval esrc input in
      replace_interval expr itv input
    | LO.Call | LO.Invoke ->
      (* TODO: function call for inter-procedural analysis *)
      let callee = callee_of_instr_func_call ins in
      let fname = func_name callee in
      let _ = hprint "Function name: " pr_str fname in
      if String.equal fname __assume_range
      then (
        let v = operand ins 0 in
        let lb_opt = int64_of_const (operand ins 1) in
        let ub_opt = int64_of_const (operand ins 2) in
        let assume_itv =
          match lb_opt, ub_opt with
          | None, _ -> Bottom
          | _, None -> Bottom
          | Some lb, Some ub ->
            mk_interval_range ~li:true ~ui:true (Int64 lb) (Int64 ub) in
        let assume_expr = expr_of_instr (mk_instr v) in
        let original_itv = get_interval assume_expr input in
        replace_interval assume_expr
          (intersect_interval original_itv assume_itv)
          input)
      else (*     print "TODO: need to handle func call: scanf" in *)
        input
    | _ -> input
  ;;
end

(*******************************************************************
 ** Main analysis module
 *******************************************************************)

module Analysis = struct
  include RangeTransfer
  include DF.MakeForwardDataFlow (RangeTransfer)
  module RU = RangeUtil
  module RT = RangeTransfer

  let get_interval (e : expr) (d : t) : interval = RU.get_interval e d
  let pr_interval (i : interval) = pr_interval i
  let pr_bound (b : bound) : string = pr_bound b

  let pr_interval_concise (i : interval) : string =
    match i with
    | Bottom -> "[Empty]"
    | Range r ->
      if r.range_lb_incl && r.range_ub_incl && r.range_lb == r.range_ub
      then pr_bound r.range_ub
      else pr_range r
  ;;
end
