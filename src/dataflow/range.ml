(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore
open Llir

module AS = Assertion
module DF = Dataflow
module LL = Llvm
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LV = LL.ValueKind
module LA = List.Assoc
module BG = Bug
module SP = Set.Poly
module MP = Map.Poly

type big_int = Big_int.big_int

(* TODO: refactoring: extract this library as an independent library *)
module BInt = struct
  include Big_int

  let one = Big_int.unit_big_int

  let zero = Big_int.zero_big_int

  let subtract = Big_int.sub_big_int

  let neg = Big_int.minus_big_int

  let compute_lower_bound_two_complement (n : int) : big_int =
    let x = Big_int.power_int_positive_int 2 (n - 1) in
    neg x

  let compute_upper_bound_two_complement (n : int) : big_int =
    let x = Big_int.power_int_positive_int 2 (n - 1) in
    subtract x one

  (** return lower bound, upper bound of a n bits two's complement number *)
  let compute_range_two_complement (n : int) : big_int * big_int =
    let x = Big_int.power_int_positive_int 2 (n - 1) in
    let lb = neg x in
    let ub = subtract x one in
    (lb, ub)

end

(*******************************************************************
 ** Abstract domain for the analysis
 *******************************************************************)

module IntervalDomain = struct

  type bound =
    | PInf                         (* positive infinity *)
    | NInf                         (* negative infiinity *)
    (* | PTwoPower of int             (\* +(2^n) or 2^n for short, with n>=0 *\) *)
    (* | NTwoPower of int             (\* -(2^n): negative number, with n>=0 *\) *)
    | Int64 of int64               (* integer 64 bit *)
    | BInt of big_int

  type range = {
    range_lb : bound;             (* lower bound *)
    range_li : bool;              (* lower inclusive *)
    range_ub : bound;             (* upper bound *)
    range_ui : bool;              (* upper inclusive *)
  }

  type interval =
    | Bottom
    | Range of range

  type einterval = {
    ei_expr : expr;
    ei_interval : interval;
  }

  (* printers *)

  let pr_bound (b: bound) : string =
    match b with
    | PInf -> "+inf"
    | NInf -> "-inf"
    | Int64 x -> Int64.to_string x
    | BInt x -> BInt.string_of_big_int x

  let pr_range (r: range) : string =
    let plb = if r.range_li then "[" else "(" in
    let pub = if r.range_ui then "]" else ")" in
    let lb = pr_bound r.range_lb in
    let ub = pr_bound r.range_ub in
    plb ^ lb ^ ", " ^ ub ^ pub

  let pr_interval (i: interval) : string =
    match i with
    | Bottom -> "Bottom"
    | Range r -> pr_range r

  let pr_einterval (e: einterval) : string =
    (pr_expr e.ei_expr) ^ ": " ^ (pr_interval e.ei_interval)

  (* constructors *)

  let mk_range ?(li=false) ?(ui=false) lb ub =
    (* normalize integer range to have inclusive lower && upper bounds *)
    let lb, li = match lb, li with
      | Int64 x, false -> Int64 (Int64.(+) x Int64.one), true
      | _ -> lb, li in
    let ub, ui = match ub, ui with
      | Int64 x, false -> Int64 (Int64.(-) x Int64.one), true
      | _ -> ub, ui in
    { range_lb = lb;
      range_li = li;
      range_ub = ub;
      range_ui = ui; }

  let mk_interval_range ?(li=false) ?(ui=false) lb ub =
    Range (mk_range ~li ~ui lb ub)

  let mk_interval_bottom () =
    Bottom

  let mk_einterval (e: expr) (i: interval) : einterval =
    { ei_expr = e;
      ei_interval = i; }

  (* utilities *)

  let range_of_bound (c: bound) : range =
    mk_range c c ~li:true ~ui:true

  let interval_of_bound (c: bound) : interval =
    Range (range_of_bound c)

  (* constant *)

  let least_interval = mk_interval_bottom ()

  let greatest_interval = mk_interval_range NInf PInf

  (* comparison *)

  let compare_bound (a: bound) (b: bound) : int =
    match a, b with
    (* either a, or b is PInf *)
    | PInf, PInf -> 0
    | PInf, _ -> 1
    | _, PInf -> -1
    (* either a, or b is NInf *)
    | NInf, NInf -> 0
    | NInf, _ -> -1
    | _, NInf -> 1
    (* one of a, b is Int64, or Big_int *)
    | Int64 x, Int64 y -> Int64.compare x y
    | BInt x, BInt y -> BInt.compare_big_int x y
    | Int64 x, BInt y -> BInt.compare_big_int (BInt.big_int_of_int64 x) y
    | BInt x, Int64 y -> BInt.compare_big_int x (BInt.big_int_of_int64 y)

  (* leq *)

  let lequal_bound (a: bound) (b: bound) : bool =
    compare_bound a b <= 0

  let lequal_range (a: range) (b: range) : bool =
    (* assume that the ranges are normalized with inclusive integer bounds *)
    lequal_bound b.range_lb a.range_lb &&
    lequal_bound a.range_ub b.range_ub &&
    Bool.equal a.range_li b.range_li &&
    Bool.equal a.range_ui b.range_ui

  let lequal_interval (a: interval) (b: interval) : bool =
    match a, b with
    | Bottom, _ -> true
    | Range _, Bottom -> false
    | Range ra, Range rb -> lequal_range ra rb

  let lequal_einterval (a: einterval) (b: einterval) : bool =
    equal_expr a.ei_expr b.ei_expr &&
    lequal_interval a.ei_interval b.ei_interval

  (* equal *)

  let equal_bound (a: bound) (b: bound) : bool =
    compare_bound a b = 0

  let equal_range (a: range) (b: range) : bool =
    (* assume that the ranges are normalized with inclusive integer bounds *)
    equal_bound b.range_lb a.range_lb &&
    equal_bound a.range_ub b.range_ub &&
    Bool.equal a.range_li b.range_li &&
    Bool.equal a.range_ui b.range_ui

  let equal_interval (a: interval) (b: interval) : bool =
    match a, b with
    | Bottom, Bottom -> true
    | Range ra, Range rb -> equal_range ra rb
    | _ -> false

  let equal_einterval (a: einterval) (b: einterval) : bool =
    equal_expr a.ei_expr b.ei_expr &&
    equal_interval a.ei_interval b.ei_interval

  (* operations with bound *)

  (** add two bounds, return output bound *)
  let add_bound (a: bound) (b: bound) : bound =
    match a, b with
    (* either a, b is PInf *)
    | PInf, NInf -> error "add_bound: undefined for PInf + NInf"
    | NInf, PInf -> error "add_bound: undefined for NInf + PInf"
    | PInf, _ -> PInf
    | _, PInf -> PInf
    (* either a, b is NInf *)
    | NInf, _ -> NInf
    | _, NInf -> NInf
    (* Int64, Big_int *)
    | Int64 x, Int64 y -> Int64 (Int64.(+) x y)
    | BInt x, BInt y -> BInt (BInt.add_big_int x y)
    | Int64 x, BInt y -> BInt (BInt.add_big_int (BInt.big_int_of_int64 x) y)
    | BInt x, Int64 y -> BInt (BInt.add_big_int x (BInt.big_int_of_int64 y))

  let sub_bound (a: bound) (b: bound) =
    let neg_b = match b with
      | PInf -> NInf
      | NInf -> PInf
      | Int64 x -> Int64 (Int64.neg x)
      | BInt x -> BInt (BInt.neg x) in
    add_bound a neg_b

  let mult_int_bound (i: int64) (b: bound) =
    match b with
    | PInf ->
      if Int64.(=) i Int64.zero then error "mult_bound: 0 * PInf is undefined"
      else if Int64.is_positive i then PInf
      else NInf
    | NInf ->
      if Int64.(=) i Int64.zero then error "mult_bound: 0 * NInf is undefined"
      else if Int64.is_positive i then NInf
      else PInf
    | Int64 x -> Int64 (Int64.( * ) i x)
    | BInt x -> BInt (BInt.mult_big_int (BInt.big_int_of_int64 i) x)

  let mult_big_int_bound (i: big_int) (b: bound) =
    match b with
    | PInf ->
      if BInt.eq_big_int i BInt.zero then
        error "mult_bound: 0 * PInf is undefined"
      else if BInt.gt_big_int i BInt.zero then PInf
      else NInf
    | NInf ->
      if BInt.eq_big_int i BInt.zero then
        error "mult_bound: 0 * NInf is undefined"
      else if BInt.gt_big_int i BInt.zero then NInf
      else PInf
    | Int64 x -> BInt (BInt.mult_big_int i (BInt.big_int_of_int64 x))
    | BInt x -> BInt (BInt.mult_big_int i x)

  let mult_bound (a: bound) (b: bound) =
    match a, b with
    | PInf, NInf -> error "mult_bound: PInf * NInf is undefined"
    | NInf, PInf -> error "mult_bound: NInf * PInf is undefined"
    | PInf, PInf -> PInf
    | NInf, NInf -> PInf
    | Int64 x, _ -> mult_int_bound x b
    | _, Int64 x -> mult_int_bound x a
    | BInt x, _ -> mult_big_int_bound x b
    | _, BInt x -> mult_big_int_bound x a

  let add_range (a: range) (b: range) =
    let lb = add_bound a.range_lb b.range_lb in
    let li = a.range_li && b.range_li in
    let ub = add_bound a.range_ub b.range_ub in
    let ui = a.range_ui && b.range_ui in
    mk_range lb ub ~li ~ui

  let sub_range (a: range) (b: range) =
    let lb = sub_bound a.range_lb b.range_ub in
    let li = a.range_li && b.range_ui in
    let ub = sub_bound a.range_ub b.range_lb in
    let ui = a.range_ui && b.range_li in
    mk_range lb ub ~li ~ui

  let mult_range (a: range) (b: range) =
    let get_upper_bound (b1, i1) (b2, i2) =
      if compare_bound b1 b2 > 0 then (b1, i1)
      else if compare_bound b1 b2 < 0 then (b2, i2)
      else b1, i1 || i2 in
    let get_lower_bound (b1, i1) (b2, i2) =
      if compare_bound b1 b2 < 0 then (b1, i1)
      else if compare_bound b1 b2 > 0 then (b2, i2)
      else b1, i1 || i2 in
    let b1, i1 = mult_bound a.range_lb b.range_lb, a.range_li && b.range_li in
    let b2, i2 = mult_bound a.range_lb b.range_ub, a.range_li && b.range_ui in
    let b3, i3 = mult_bound a.range_ub b.range_lb, a.range_ui && b.range_li in
    let b4, i4 = mult_bound a.range_ub b.range_ub, a.range_ui && b.range_ui in
    let lb, li = (b1, i1) |> get_lower_bound (b2, i2) |>
                 get_lower_bound (b3, i3) |> get_lower_bound (b4, i4) in
    let ub, ui = (b1, i1) |> get_upper_bound (b2, i2) |>
                 get_upper_bound (b3, i3) |> get_upper_bound (b4, i4) in
    mk_range lb ub ~li ~ui

  let union_range (a: range) (b: range) : range =
    let lb, li =
      let cmp = compare_bound a.range_lb b.range_lb in
      if cmp < 0 then (a.range_lb, a.range_li)
      else if cmp > 0 then (b.range_lb, b.range_li)
      else (a.range_lb, a.range_li && b.range_li) in
    let ub, ui =
      let cmp = compare_bound a.range_ub b.range_ub in
      if cmp > 0 then (a.range_ub, a.range_ui)
      else if cmp < 0 then (b.range_ub, b.range_ui)
      else (a.range_ub, a.range_ui && b.range_ui) in
    mk_range lb ub ~li ~ui

  let widen_range (a: range) (b: range) : range =
    let lb, li =
      if compare_bound a.range_lb b.range_lb <= 0 then
        (a.range_lb, a.range_li)
      else (NInf, false) in
    let ub, ui =
      if compare_bound a.range_ub b.range_ub >= 0 then
        (a.range_ub, a.range_ui)
      else (PInf, false) in
    mk_range lb ub ~li ~ui

  let combine_range ?(widen=false) (a: range) (b: range) : range =
    if widen then widen_range a b
      (* union_range a b *)
      (* widen_range a b *)   (* FIXME: temporarily disable widening *)
    else union_range a b

  let add_interval (a: interval) (b: interval) =
    match a, b with
    | Bottom, _ -> Bottom
    | _, Bottom -> Bottom
    | Range ra, Range rb -> Range (add_range ra rb)

  let sub_interval (a: interval) (b: interval) =
    match a, b with
    | Bottom, _ -> Bottom
    | _, Bottom -> Bottom
    | Range ra, Range rb -> Range (sub_range ra rb)

  let mult_interval (a: interval) (b: interval) =
    match a, b with
    | Bottom, _ -> Bottom
    | _, Bottom -> Bottom
    | Range ra, Range rb -> Range (mult_range ra rb)

  let intersect_interval (a: interval) (b: interval) : interval =
    match a, b with
    | Bottom, _ -> Bottom
    | _, Bottom -> Bottom
    | Range ra, Range rb ->
      let lb, li =
        let cmp = compare_bound ra.range_lb rb.range_lb in
        if cmp > 0 then (ra.range_lb, ra.range_li)
        else if cmp < 0 then (rb.range_lb, rb.range_li)
        else (ra.range_lb, ra.range_li && rb.range_li) in
      let ub, ui =
        let cmp = compare_bound ra.range_ub rb.range_ub in
        if cmp < 0 then (ra.range_ub, ra.range_ui)
        else if cmp > 0 then (rb.range_ub, rb.range_ui)
        else (ra.range_ub, ra.range_ui && rb.range_ui) in
      if compare_bound lb ub > 0 then Bottom
      else Range (mk_range lb ub ~li ~ui)

  let combine_interval ?(widen=false) (a: interval) (b: interval) : interval =
    match a, b with
    | Bottom, _ -> b
    | _, Bottom -> a
    | Range ra, Range rb -> Range (combine_range ~widen ra rb)

  let extract_constant_bound (e: expr) : bound option =
    match e with
    | Int64 i -> Some (Int64 i)
    | _ -> None

  let compare_range_upper_bound_with_int (r: range) (i: int64) : int =
    match r.range_ub with
    | PInf -> 1
    | NInf -> -1
    | Int64 x ->
      let cmp = Int64.compare x i in
      if r.range_ui || cmp != 0 then cmp
      else -1
    | BInt x ->
      let cmp = BInt.compare_big_int x (BInt.big_int_of_int64 i) in
      if r.range_ui || cmp != 0 then cmp
      else -1

  let compare_interval_upper_bound_with_int (it: interval) (i: int64) : int =
    match it with
    | Bottom -> -1
    | Range r -> compare_range_upper_bound_with_int r i

end;;

(*******************************************************************
 ** Core data transfer modules
 *******************************************************************)

module IntervalData = struct
  open IntervalDomain

  (* FIXME: use Map to represent it *)
  type t = (expr, interval) MP.t

  (* type t = einterval list         (\* sorted by expr *\) *)

end

module RangeTransfer : DF.ForwardDataTransfer= struct
  open IntervalDomain

  include IntervalData
  include DF.DataUtilGenerator(IntervalData)

  let analysis = DfaRange

  (*******************************************************************
   ** Handling abstract data
   *******************************************************************)

  (* constants *)

  let least_data : t = MP.empty

  (* printers *)

  let pr_data (d: t) =
    let expr_interval_lst = MP.to_alist ~key_order:`Decreasing d in
    pr_list_square (fun (e, i) ->
      (pr_expr e) ^ ": " ^ (pr_interval i)) expr_interval_lst

  let pr_data_checksum = pr_data

  (* comparison*)

  let equal_data (a: t) (b: t) : bool =
    MP.equal equal_interval a b

  let lequal_data (a: t)  (b: t) : bool =
    MP.for_alli ~f:(fun ~key:ea ~data:ia ->
      MP.existsi ~f:(fun ~key:eb ~data:ib ->
        equal_expr ea eb &&
        lequal_interval ia ib
      ) b
    ) a

  let copy_data d =
    d

  (* substitution *)

  let subst_data ?(sstv: substv = []) ?(sstve: substve = [])
        ?(sste: subste = []) (d: t) : t =
    MP.fold ~f:(fun ~key:e ~data:i acc ->
      let ne = subst_expr ~sstv ~sstve ~sste e in
      match MP.add acc ~key:ne ~data:i with
      | `Ok res -> res
      | `Duplicate ->
        let _ = warning ("Range.subst_data: duplicate expr after substitution" ^
                         (pr_expr ne)) in
        acc) ~init:MP.empty d

  let merge_data ?(widen=false) (a: t) (b: t) : t =
    MP.merge ~f:(fun ~key:e i -> match i with
      | `Both (ia, ib) -> Some (combine_interval ia ib)
      | `Left ia -> Some ia
      | `Right ib -> Some ib) a b

  (* FIXME: fix this later *)
  let join_data (a: t) (b: t) : t =
    merge_data a b

  let get_interval (e: expr) (d: t) : interval =
    match e with
    | Int64 i -> interval_of_bound (Int64 i)
    | _ ->
      match MP.find d e with
      | Some i -> i
      | None -> least_interval

  let replace_interval (e: expr) (i: interval) (d: t) : t =
    let nd = match MP.find d e with
      | None -> d
      | Some a -> MP.remove d e in
    MP.add_exn ~key:e ~data:i nd

  let clean_irrelevant_info_from_data penv func (d: t) : t =
    MP.filteri ~f:(fun ~key:e ~data:i ->
      let vs = collect_llvalue_of_expr e in
      not (List.exists ~f:is_local_llvalue vs)) d

  let clean_info_of_vars (input: t) (vs: llvalues) : t =
    (* TODO: implement later *)
    input

  let extract_data_from_predicate ~(widen: bool) (p: predicate) (d: t) : t =
    match p with
    | PBool _ -> MP.empty
    | PIcmp (cmp, lhs, rhs) ->
      let lhs, rhs = expr_of_llvalue lhs, expr_of_llvalue rhs in
      if is_constant_expr lhs && not (is_constant_expr rhs) then (
        let b = match extract_constant_bound lhs with
          | Some b -> b
          | None -> herror "extract_const_bound_lhs: not found:" pr_expr lhs in
        match cmp with
        | LC.Eq -> MP.of_alist_exn [(rhs, interval_of_bound b)]
        | LC.Ne -> MP.empty  (* TODO: can be improved here to be more precise *)
        | LC.Ugt | LC.Sgt ->
          MP.of_alist_exn [(rhs, mk_interval_range ~ui:false NInf b)]
        | LC.Uge | LC.Sge ->
          MP.of_alist_exn [(rhs, mk_interval_range ~ui:true NInf b)]
        | LC.Ult | LC.Slt ->
          MP.of_alist_exn [(rhs, mk_interval_range ~li:false b PInf)]
        | LC.Ule | LC.Sle ->
          MP.of_alist_exn [(rhs, mk_interval_range ~li:true b PInf)])
      else if not (is_constant_expr lhs) && is_constant_expr rhs then (
        let b = match extract_constant_bound rhs with
          | Some b -> b
          | None -> herror "extract_const_bound_rhs: not found:" pr_expr rhs in
        match cmp with
        | LC.Eq -> MP.of_alist_exn [lhs, interval_of_bound b]
        | LC.Ne -> MP.empty  (* TODO: can be improved here to be more precise *)
        | LC.Ugt | LC.Sgt ->
          MP.of_alist_exn [lhs, mk_interval_range ~li:false b PInf]
        | LC.Uge | LC.Sge ->
          MP.of_alist_exn [lhs, mk_interval_range ~li:true b PInf]
        | LC.Ult | LC.Slt ->
          MP.of_alist_exn [lhs, mk_interval_range ~ui:false NInf b]
        | LC.Ule | LC.Sle ->
          MP.of_alist_exn [lhs, mk_interval_range ~ui:true NInf b])
      else (
        let ilhs, irhs = get_interval lhs d, get_interval rhs d in
        match cmp with
        | LC.Eq -> MP.of_alist_exn [(lhs, irhs); (rhs, ilhs)]
        | LC.Ne -> MP.empty  (* TODO: can be improved here to be more precise *)
        | LC.Ugt | LC.Sgt ->
          let nilhs = match irhs with
            | Range r ->
              if widen then mk_interval_range ~li:false r.range_lb PInf
              else mk_interval_range ~li:false r.range_ub PInf
            | Bottom -> Bottom in
          let nirhs = match ilhs with
            | Range r ->
              if widen then mk_interval_range ~ui:false NInf r.range_ub
              else mk_interval_range ~ui:false NInf r.range_lb
            | Bottom -> Bottom in
          MP.of_alist_exn [(lhs, nilhs); (rhs, nirhs)]
        | LC.Uge | LC.Sge ->
          let nilhs = match irhs with
            | Range r ->
              if widen then mk_interval_range ~li:true r.range_lb PInf
              else mk_interval_range ~li:true r.range_ub PInf
            | Bottom -> Bottom in
          let nirhs = match ilhs with
            | Range r ->
              if widen then mk_interval_range ~ui:true NInf r.range_ub
              else mk_interval_range ~ui:true NInf r.range_lb
            | Bottom -> Bottom in
          MP.of_alist_exn [(lhs, nilhs); (rhs, nirhs)]
        | LC.Ult | LC.Slt ->
          let nilhs = match irhs with
            | Range r ->
              if widen then mk_interval_range ~ui:false NInf r.range_ub
              else mk_interval_range ~ui:false NInf r.range_lb
            | Bottom -> Bottom in
          let nirhs = match ilhs with
            | Range r ->
              if widen then mk_interval_range ~li:false r.range_lb PInf
              else mk_interval_range ~li:false r.range_ub PInf
            | Bottom -> Bottom in
          MP.of_alist_exn [(lhs, nilhs); (rhs, nirhs)]
        | LC.Ule | LC.Sle ->
          let nilhs = match irhs with
            | Range r ->
              if widen then mk_interval_range ~ui:true NInf r.range_ub
              else mk_interval_range ~ui:true NInf r.range_lb
            | Bottom -> Bottom in
          let nirhs = match ilhs with
            | Range r ->
              if widen then mk_interval_range ~li:true r.range_lb PInf
              else mk_interval_range ~li:true r.range_ub PInf
            | Bottom -> Bottom in
          MP.of_alist_exn [(lhs, nilhs); (rhs, nirhs)])
    | PFcmp _ -> MP.empty
    | PNeg _ | PConj _ | PDisj _ ->
      herror "extract_data_from_predicate: need to handle: " pr_pred p

  let is_data_satisfied_predicate (d: t) (p: predicate) : bool =
    let pdata = extract_data_from_predicate ~widen:false p d in
    MP.for_alli ~f:(fun ~key:v ~data:vip ->
      let vid = get_interval v d in
      lequal_interval vid vip) pdata

  let refine_data_by_predicate ?(widen=false) (d: t) (p: predicate) : t =
    let refine_interval (a: interval) (b: interval) : interval =
      match a, b with
      | Bottom, _ -> Bottom
      | _, Bottom -> Bottom
      | Range ra, Range rb ->
        let lb, li =
          let cmp = compare_bound ra.range_lb rb.range_lb in
          if cmp > 0 then (ra.range_lb, ra.range_li)
          else if cmp < 0 then (ra.range_lb, ra.range_li)
          else (ra.range_lb, ra.range_li && rb.range_li) in
        let ub, ui =
          let cmp = compare_bound ra.range_ub rb.range_ub in
          if cmp < 0 then (ra.range_ub, ra.range_ui)
          else if cmp > 0 then (ra.range_ub, ra.range_ui)
          else (ra.range_ub, ra.range_ui && rb.range_ui) in
        if compare_bound lb ub > 0 then Bottom
        else Range (mk_range lb ub ~li ~ui) in
    let pcond_data = extract_data_from_predicate ~widen p d in
    MP.merge ~f:(fun ~key:e i -> match i with
      | `Both (id, ip) -> Some (intersect_interval id ip)
      | `Left id -> Some id
      | `Right _ -> None) d pcond_data

    (* List.fold_left ~f:(fun acc ep ->
     *   let e = ep.ei_expr in
     *   match List.find ~f:(fun a -> equal_expr e a.ei_expr) acc with
     *   | None -> acc
     *   | Some ed ->
     *     let nacc = List.exclude acc ~f:(fun a -> equal_expr e a.ei_expr) in
     *     let nint = intersect_interval ep.ei_interval ed.ei_interval in
     *     (mk_einterval e nint)::nacc) ~init:d pcond_data *)

  (*******************************************************************
   ** Core analysis functions
   *******************************************************************)

  let need_widening func : bool =
    true

  let refine_widening func instr widen : bool =
    let users = get_users (llvalue_of_instr instr) in
    let has_int_cmp_relation = List.exists ~f:(fun u ->
      match LL.classify_value u with
      | LV.Instruction LO.ICmp -> false
      | _ -> true) users in
    if not has_int_cmp_relation then widen
    else true

  let prepare_callee_input penv instr func args (input: t) : t =
    input

  let compute_callee_output_exns penv instr callee args input fsum : t * exns =
    (input, [])

  let prepare_thrown_exception_data penv exn_ptr tinfo input : t =
    input

  let compute_catch_exception_data penv instr ptr input exn : t =
    input

  let analyze_global (g: global) (input: t) : t =
    (* TODO: default behavior, implement later if necessary *)
    input

  let analyze_instr ?(widen=false) (penv: prog_env) fenv instr (input: t) : t =
    let func = func_of_instr instr in
    let expr = expr_of_instr instr in
    match instr_opcode instr with
    | LO.Unreachable -> least_data
    | LO.Add ->
      let opr0, opr1 = expr_operand instr 0, expr_operand instr 1 in
      let itv0, itv1 = get_interval opr0 input, get_interval opr1 input in
      let itv = add_interval itv0 itv1 in
      replace_interval expr itv input
    | LO.Sub ->
      let opr0, opr1 = expr_operand instr 0, expr_operand instr 1 in
      let itv0, itv1 = get_interval opr0 input, get_interval opr1 input in
      let itv = sub_interval itv0 itv1 in
      replace_interval expr itv input
    | LO.Mul ->
      let opr0, opr1 = expr_operand instr 0, expr_operand instr 1 in
      let itv0, itv1 = get_interval opr0 input, get_interval opr1 input in
      let itv = mult_interval itv0 itv1 in
      replace_interval expr itv input
    | LO.UDiv | LO.SDiv ->
      (* TODO: need to handle Div *)
      input
    | LO.PHI ->
      let opr0 = expr_operand instr 0 in
      let itv = ref (get_interval opr0 input) in
      let _ = hdebug "Before refine widening: " pr_bool widen in
      let widen = refine_widening func instr widen in
      let _ = hdebug "After refine widening: " pr_bool widen in
      for i = 1 to (num_operands instr) - 1 do
        let opri = expr_operand instr i in
        let itvi = get_interval opri input in
        (* TODO: need to refine widening here *)
        itv := combine_interval ~widen !itv itvi
      done;
      replace_interval (expr_of_instr instr) !itv input
    | LO.Ret ->
      let expr = mk_expr_func_result func in
      let opr0 = expr_operand instr 0 in
      let itv = get_interval opr0 input in
      replace_interval expr itv input
    | LO.Load ->
      let dst = dst_of_instr_load instr in
      let dtyp = LL.type_of dst in
      if is_type_integer dtyp then
        let bw = LL.integer_bitwidth dtyp in
        (* LLVM integers is represented in two's complement format *)
        let lb, ub = BInt.compute_range_two_complement bw in
        let ir = mk_interval_range ~li:true ~ui:true (BInt lb) (BInt ub) in
        replace_interval expr ir input
      else input
    | LO.Store -> input
    | LO.ZExt ->
      let esrc = expr_of_llvalue (src_of_instr_zext instr) in
      let itv = get_interval esrc input in
      replace_interval expr itv input
    | LO.Call | LO.Invoke ->
      (* TODO: function call for inter-procedural analysis *)
      (* let callee = callee_of_callable_instr instr in *)
      (* let _ = if is_func_scanf callee then *)
      (*     print "TODO: need to handle func call: scanf" in *)
      input
    | _ -> input

  (*******************************************************************
   ** Checking bugs
   *******************************************************************)

  let check_buffer_overflow fenv (bof: BG.buffer_overflow) : ternary =
    if !bug_memory_all || !bug_buffer_overflow then
      match get_instr_output fenv bof.bof_instr with
      | None -> False
      | Some data ->
        let itv = get_interval (expr_of_llvalue bof.bof_elem_index) data in
        match bof.bof_buff_size with
        | None -> False
        | Some (Int64 n) ->
          if compare_interval_upper_bound_with_int itv n >= 0 then True
          else False
        | _ -> False  (* TODO: check symbolic size *)
    else False

  let check_integer_overflow fenv (iof: BG.integer_overflow) : ternary =
    if !bug_integer_all || !bug_integer_overflow then
      match get_instr_output fenv iof.iof_instr with
      | None -> Unkn
      | Some data ->
        let itv = get_interval (expr_of_llvalue iof.iof_expr) data in
        match itv with
        | Bottom -> Unkn
        | Range r ->
          let ub = BInt.compute_upper_bound_two_complement iof.iof_bitwidth in
          if compare_bound r.range_ub (BInt ub) > 0 then True
          else False
    else Unkn

  let check_integer_underflow fenv (iuf: BG.integer_underflow) : ternary =
    if !bug_integer_all || !bug_integer_underflow then
      match get_instr_output fenv iuf.iuf_instr with
      | None -> Unkn
      | Some data ->
        let itv = get_interval (expr_of_llvalue iuf.iuf_expr) data in
        match itv with
        | Bottom -> Unkn
        | Range r ->
          let lb = BInt.compute_lower_bound_two_complement iuf.iuf_bitwidth in
          if compare_bound r.range_lb (BInt lb) < 0 then True
          else False
    else Unkn

  let check_bug (fenv: func_env) (bug: BG.bug) : ternary =
    match bug.BG.bug_type with
    | BG.BufferOverflow bof -> check_buffer_overflow fenv bof
    | BG.IntegerOverflow iof -> check_integer_overflow fenv iof
    | BG.IntegerUnderflow iuf -> check_integer_underflow fenv iuf
    | _ -> Unkn

  (*******************************************************************
   ** Propagate bug infor
   *******************************************************************)

  (* let propagate_buffer_overflow_info fenv (bof: BG.buffer_overflow) : BG.buffer_overflow = *)
  (*   if !bug_memory_all || !bug_buffer_overflow then *)
  (*     match get_instr_output fenv bof.bof_instr with *)
  (*     | None -> bof *)
  (*     | Some data -> *)
  (*       let itv = get_interval (expr_of_llvalue bof.bof_elem_index) data in *)
  (*       match bof.bof_buff_size with *)
  (*       | None -> False *)
  (*       | Some (Int64 n) -> *)
  (*         if compare_interval_upper_bound_with_int itv n >= 0 then True *)
  (*         else False *)
  (*       | _ -> False  (\* TODO: check symbolic size *\) *)
  (*   else bof *)

  (* let propage_bug_infor (fenv: func_env) (bug: BG.bug) : BG.bug = *)
  (*   match bug.BG.bug_type with *)
  (*   | BG.BufferOverflow bof -> check_buffer_overflow fenv bof *)
  (*   (\* | BG.IntegerOverflow iof -> check_integer_overflow fenv iof *\) *)
  (*   (\* | BG.IntegerUnderflow iuf -> check_integer_underflow fenv iuf *\) *)
  (*   | _ -> Unkn *)

  (*******************************************************************
   ** Checking assertions
   *******************************************************************)

  let count_assertions (prog: program) : int =
    (* TODO: implement later if necessary *)
    let assertions = List.fold_left ~f:(fun acc func ->
      acc @(AS.find_range_assertions func)) ~init:[] prog.prog_user_funcs in
    List.length assertions

  let check_range_lower_bound fenv instr (v: llvalue) (lb: int64) : bool =
    match get_instr_output fenv instr with
    | None -> false
    | Some data ->
      match get_interval (expr_of_llvalue v) data with
      | Bottom -> false
      | Range r ->
        match r.range_lb with
        | PInf -> true
        | NInf -> false
        | Int64 i ->
          let vlb =
            if r.range_li then i
            else Int64.(+) i Int64.one in
          Int64.(>=) vlb lb
        | BInt i ->
          let vlb =
            if r.range_li then i
            else BInt.add_big_int i BInt.one in
          BInt.ge_big_int vlb (BInt.big_int_of_int64 lb)

  let check_range_upper_bound fenv instr (v: llvalue) (ub: int64) : bool =
    match get_instr_output fenv instr with
    | None -> false
    | Some data ->
      match get_interval (expr_of_llvalue v) data with
      | Bottom -> true
      | Range r ->
        match r.range_ub with
        | PInf -> false
        | NInf -> true
        | Int64 i ->
          let vub = if r.range_ui then i else Int64.(-) i Int64.one in
          Int64.(<=) vub ub
        | BInt i ->
          let vub = if r.range_ui then i else BInt.sub_big_int i BInt.one in
          BInt.le_big_int vub (BInt.big_int_of_int64 ub)

  let check_range_full fenv instr (v: llvalue) (lb: int64) (ub: int64) : bool =
    (check_range_lower_bound fenv instr v lb) &&
    (check_range_upper_bound fenv instr v ub)

  let check_assertion (fenvs: func_env list) (ast: AS.assertion) : bool option =
    let instr = ast.AS.ast_instr in
    match ast.AS.ast_type, ast.AS.ast_predicate with
    | AS.Assert, AS.RangeLB (v::lb::_) -> (
        match LL.int64_of_const lb with
        | None -> None
        | Some lb ->
          Some (List.exists ~f:(fun fe ->
            check_range_lower_bound fe instr v lb) fenvs))
    | AS.Assert, AS.RangeUB (v::ub::_) -> (
        match LL.int64_of_const ub with
        | None -> None
        | Some ub ->
          Some (List.exists ~f:(fun fe ->
            check_range_upper_bound fe instr v ub) fenvs))
    | AS.Assert, AS.RangeFull (v::lb::ub::_) -> (
        match LL.int64_of_const lb, LL.int64_of_const ub with
        | None, _ | _, None -> None
        | Some lb, Some ub ->
          Some (List.exists ~f:(fun fe ->
            check_range_full fe instr v lb ub) fenvs))
    | _ -> None

  let check_assertions (penv: prog_env) func : int =
    let prog = penv.penv_prog in
    let assertions = AS.find_range_assertions func in
    let fenvs = match Hashtbl.find penv.penv_func_envs func with
      | None -> []
      | Some fenvs -> fenvs in
    let num_checked_assertions = ref 0 in
    let results = List.iter ~f:(fun ast ->
      match check_assertion fenvs ast with
      | Some res ->
        let _ = incr num_checked_assertions in
        let _ = incr (if res then num_valid_asserts else num_invalid_asserts) in
        print_endline (AS.pr_assertion_status func ast res)
      | None -> ()) assertions in
    !num_checked_assertions

end

(*******************************************************************
 ** Main analysis module
 *******************************************************************)

module RangeAnalysis = struct
  include RangeTransfer
  include DF.ForwardDataFlow(RangeTransfer)

  module RT = RangeTransfer
  module ID = IntervalDomain

  (* TODO: how to expose some internal function here? *)

  (* let get_interval (e: expr) (d: t) : ID.interval = *)
  (*   match e with *)
  (*   | Int64 i -> ID.interval_of_bound (Int64 i) *)
  (*   | _ -> *)
  (*     match MP.find d e with *)
  (*     | Some i -> i *)
  (*     | None -> IntervalDomain.least_interval *)


end
