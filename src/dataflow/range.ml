(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

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

(*******************************************************************
 ** Abstract domain for the analysis
 *******************************************************************)

module IntervalDomain = struct
  type bound =
    | PInf (* positive infinity *)
    | NInf (* negative infinity *)
    | Int64 of int64 (* integer 64 bit *)
    | EInt of eint
    | BInt of bint

  (* lower bound, upper bound, and bound inclusiveness *)
  type range =
    { range_lb : bound;
      range_lb_incl : bool;
      range_ub : bound;
      range_ub_incl : bool
    }

  type interval =
    | Bottom
    | Range of range

  (* printers *)

  let pr_bound (b : bound) : string =
    match b with
    | PInf -> "+inf"
    | NInf -> "-inf"
    | Int64 x -> Int64.to_string x
    | EInt x -> EInt.pr_eint x
    | BInt x -> BInt.pr_bint x
  ;;

  let pr_range (r : range) : string =
    let plb = if r.range_lb_incl then "[" else "(" in
    let pub = if r.range_ub_incl then "]" else ")" in
    let lb = pr_bound r.range_lb in
    let ub = pr_bound r.range_ub in
    plb ^ lb ^ ", " ^ ub ^ pub
  ;;

  let pr_interval (i : interval) : string =
    match i with
    | Bottom -> "Bottom"
    | Range r -> pr_range r
  ;;

  (* constructors *)

  let mk_range ?(li = false) ?(ui = false) (lb : bound) (ub : bound) =
    (* normalize integer range to have inclusive lower && upper bounds *)
    let lb, li =
      match lb, li with
      | Int64 x, false -> Int64 (Int64.( + ) x Int64.one), true
      | _ -> lb, li in
    let ub, ui =
      match ub, ui with
      | Int64 x, false -> Int64 (Int64.( - ) x Int64.one), true
      | _ -> ub, ui in
    { range_lb = lb; range_lb_incl = li; range_ub = ub; range_ub_incl = ui }
  ;;

  let mk_interval_range ?(li = false) ?(ui = false) (lb : bound) (ub : bound)
      : interval
    =
    Range (mk_range ~li ~ui lb ub)
  ;;

  let mk_interval_bottom () : interval = Bottom

  (* let compute_int_max_interval (bitwidth : int) : interval = *)
  (*   let lb, ub = BInt.compute_range_two_complement bitwidth in *)
  (*   mk_interval_range ~li:true ~ui:true (BInt lb) (BInt ub) *)
  (* ;; *)

  let compute_int_max_interval (bitwidth : int) : interval =
    let lb, ub = EInt.compute_range_two_complement bitwidth in
    mk_interval_range ~li:true ~ui:true (EInt lb) (EInt ub)
  ;;

  (* utilities *)

  let range_of_bound (b : bound) : range = mk_range b b ~li:true ~ui:true
  let interval_of_bound (b : bound) : interval = Range (range_of_bound b)

  (* constant *)

  let least_interval = mk_interval_bottom ()
  let greatest_interval = mk_interval_range NInf PInf

  (* comparison *)

  let compare_bound (a : bound) (b : bound) : int =
    match a, b with
    (* either a, or b is PInf *)
    | PInf, PInf -> 0
    | PInf, _ -> 1
    | _, PInf -> -1
    (* either a, or b is NInf *)
    | NInf, NInf -> 0
    | NInf, _ -> -1
    | _, NInf -> 1
    (* one of a, b is Int64, or BIint *)
    | Int64 x, Int64 y -> Int64.compare x y
    | Int64 x, EInt y -> BInt.compare (BInt.of_int64 x) (EInt.to_bint y)
    | Int64 x, BInt y -> BInt.compare (BInt.of_int64 x) y
    | EInt x, Int64 y -> BInt.compare (EInt.to_bint x) (BInt.of_int64 y)
    | EInt x, EInt y -> BInt.compare (EInt.to_bint x) (EInt.to_bint y)
    | EInt x, BInt y -> BInt.compare (EInt.to_bint x) y
    | BInt x, Int64 y -> BInt.compare x (BInt.of_int64 y)
    | BInt x, EInt y -> BInt.compare x (EInt.to_bint y)
    | BInt x, BInt y -> BInt.compare x y
  ;;

  (* leq *)

  let lequal_bound (a : bound) (b : bound) : bool = compare_bound a b <= 0

  let lequal_range (a : range) (b : range) : bool =
    (* assume that the ranges are normalized with inclusive integer bounds *)
    lequal_bound b.range_lb a.range_lb
    && lequal_bound a.range_ub b.range_ub
    && Bool.equal a.range_lb_incl b.range_lb_incl
    && Bool.equal a.range_ub_incl b.range_ub_incl
  ;;

  let lequal_interval (a : interval) (b : interval) : bool =
    match a, b with
    | Bottom, _ -> true
    | Range _, Bottom -> false
    | Range ra, Range rb -> lequal_range ra rb
  ;;

  (* equal *)

  let equal_bound (a : bound) (b : bound) : bool = compare_bound a b = 0

  let equal_range (a : range) (b : range) : bool =
    (* assume that the ranges are normalized with inclusive integer bounds *)
    equal_bound b.range_lb a.range_lb
    && equal_bound a.range_ub b.range_ub
    && Bool.equal a.range_lb_incl b.range_lb_incl
    && Bool.equal a.range_ub_incl b.range_ub_incl
  ;;

  let equal_interval (a : interval) (b : interval) : bool =
    match a, b with
    | Bottom, Bottom -> true
    | Range ra, Range rb -> equal_range ra rb
    | _ -> false
  ;;

  (* operations with bound *)

  (** add two bounds, return output bound *)
  let add_bound (a : bound) (b : bound) : bound =
    match a, b with
    (* either a, b is PInf *)
    | PInf, NInf -> error "add_bound: undefined for PInf + NInf"
    | NInf, PInf -> error "add_bound: undefined for NInf + PInf"
    | PInf, _ -> PInf
    | _, PInf -> PInf
    (* either a, b is NInf *)
    | NInf, _ -> NInf
    | _, NInf -> NInf
    (* Int64, BInt, EInt *)
    | Int64 x, Int64 y -> Int64 (Int64.( + ) x y)
    | Int64 x, EInt y -> EInt (EInt.add (EInt.of_int64 x) y)
    | Int64 x, BInt y -> BInt (BInt.add (BInt.of_int64 x) y)
    | EInt x, Int64 y -> EInt (EInt.add x (EInt.of_int64 y))
    | EInt x, EInt y -> EInt (EInt.add x y)
    | EInt x, BInt y -> EInt (EInt.add x (EInt.of_bint y))
    | BInt x, Int64 y -> BInt (BInt.add x (BInt.of_int64 y))
    | BInt x, EInt y -> EInt (EInt.add (EInt.of_bint x) y)
    | BInt x, BInt y -> BInt (BInt.add x y)
  ;;

  let sub_bound (a : bound) (b : bound) =
    let neg_b =
      match b with
      | PInf -> NInf
      | NInf -> PInf
      | Int64 x -> Int64 (Int64.neg x)
      | EInt x -> EInt (EInt.neg x)
      | BInt x -> BInt (BInt.neg x) in
    add_bound a neg_b
  ;;

  let udiv_bound (a : bound) (b : bound) : bound =
    match a, b with
    | PInf, PInf -> error "udiv_bound: undefined for PInf / PInf"
    | NInf, _ | _, NInf -> error "udiv_bound: does not work with NInf"
    | PInf, _ -> PInf
    | _, PInf -> BInt BInt.zero
    (* Int64, BInt, EInt *)
    | Int64 x, Int64 y -> Int64 (Int64.( / ) x y)
    | Int64 x, EInt y -> EInt (EInt.div (EInt.of_int64 x) y)
    | Int64 x, BInt y -> BInt (BInt.div (BInt.of_int64 x) y)
    | EInt x, Int64 y -> EInt (EInt.div x (EInt.of_int64 y))
    | EInt x, EInt y -> EInt (EInt.div x y)
    | EInt x, BInt y -> EInt (EInt.div x (EInt.of_bint y))
    | BInt x, Int64 y -> BInt (BInt.div x (BInt.of_int64 y))
    | BInt x, EInt y -> EInt (EInt.div (EInt.of_bint x) y)
    | BInt x, BInt y -> BInt (BInt.div x y)
  ;;

  let mult_int_bound (i : int64) (b : bound) =
    match b with
    | PInf ->
      if Int64.( = ) i Int64.zero
      then error "mult_bound: 0 * PInf is undefined"
      else if Int64.is_positive i
      then PInf
      else NInf
    | NInf ->
      if Int64.( = ) i Int64.zero
      then error "mult_bound: 0 * NInf is undefined"
      else if Int64.is_positive i
      then NInf
      else PInf
    | Int64 x -> Int64 (Int64.( * ) i x)
    | EInt x -> EInt (EInt.mult (EInt.of_int64 i) x)
    | BInt x -> BInt (BInt.mult (BInt.of_int64 i) x)
  ;;

  let mult_eint_bound (i : eint) (b : bound) =
    match b with
    | PInf ->
      let bi = EInt.to_bint i in
      if BInt.eq bi BInt.zero
      then error "mult_bound: 0 * PInf is undefined"
      else if BInt.gt bi BInt.zero
      then PInf
      else NInf
    | NInf ->
      let bi = EInt.to_bint i in
      if BInt.eq bi BInt.zero
      then error "mult_bound: 0 * NInf is undefined"
      else if BInt.gt bi BInt.zero
      then NInf
      else PInf
    | Int64 x -> EInt (EInt.mult i (EInt.of_int64 x))
    | EInt x -> EInt (EInt.mult i x)
    | BInt x -> EInt (EInt.mult i (EInt.of_bint x))
  ;;

  let mult_bint_bound (i : bint) (b : bound) =
    match b with
    | PInf ->
      if BInt.eq i BInt.zero
      then error "mult_bound: 0 * PInf is undefined"
      else if BInt.gt i BInt.zero
      then PInf
      else NInf
    | NInf ->
      if BInt.eq i BInt.zero
      then error "mult_bound: 0 * NInf is undefined"
      else if BInt.gt i BInt.zero
      then NInf
      else PInf
    | Int64 x -> BInt (BInt.mult i (BInt.of_int64 x))
    | EInt x -> EInt (EInt.mult_bint_eint i x)
    | BInt x -> BInt (BInt.mult i x)
  ;;

  let mult_bound (a : bound) (b : bound) =
    match a, b with
    | PInf, NInf -> error "mult_bound: PInf * NInf is undefined"
    | NInf, PInf -> error "mult_bound: NInf * PInf is undefined"
    | PInf, PInf -> PInf
    | NInf, NInf -> PInf
    | Int64 x, _ -> mult_int_bound x b
    | _, Int64 x -> mult_int_bound x a
    | EInt x, _ -> mult_eint_bound x b
    | _, EInt x -> mult_eint_bound x a
    | BInt x, _ -> mult_bint_bound x b
    | _, BInt x -> mult_bint_bound x a
  ;;

  let add_range (a : range) (b : range) =
    let lb = add_bound a.range_lb b.range_lb in
    let li = a.range_lb_incl && b.range_lb_incl in
    let ub = add_bound a.range_ub b.range_ub in
    let ui = a.range_ub_incl && b.range_ub_incl in
    mk_range lb ub ~li ~ui
  ;;

  let sub_range (a : range) (b : range) =
    let lb = sub_bound a.range_lb b.range_ub in
    let li = a.range_lb_incl && b.range_ub_incl in
    let ub = sub_bound a.range_ub b.range_lb in
    let ui = a.range_ub_incl && b.range_lb_incl in
    mk_range lb ub ~li ~ui
  ;;

  let udiv_range (a : range) (b : range) =
    (* TODO: Add inclusive, add Division-by-zero  *)
(*    let anew =
      if a.range_lb_incl *)
    let lb = udiv_bound a.range_lb b.range_ub in
    let ub = udiv_bound a.range_ub b.range_ub in
    mk_range lb ub
  ;;

  let mult_range (a : range) (b : range) =
    let get_upper_bound (b1, i1) (b2, i2) =
      if compare_bound b1 b2 > 0
      then b1, i1
      else if compare_bound b1 b2 < 0
      then b2, i2
      else b1, i1 || i2 in
    let get_lower_bound (b1, i1) (b2, i2) =
      if compare_bound b1 b2 < 0
      then b1, i1
      else if compare_bound b1 b2 > 0
      then b2, i2
      else b1, i1 || i2 in
    let b1, i1 =
      mult_bound a.range_lb b.range_lb, a.range_lb_incl && b.range_lb_incl
    in
    let b2, i2 =
      mult_bound a.range_lb b.range_ub, a.range_lb_incl && b.range_ub_incl
    in
    let b3, i3 =
      mult_bound a.range_ub b.range_lb, a.range_ub_incl && b.range_lb_incl
    in
    let b4, i4 =
      mult_bound a.range_ub b.range_ub, a.range_ub_incl && b.range_ub_incl
    in
    let lb, li =
      (b1, i1)
      |> get_lower_bound (b2, i2)
      |> get_lower_bound (b3, i3)
      |> get_lower_bound (b4, i4) in
    let ub, ui =
      (b1, i1)
      |> get_upper_bound (b2, i2)
      |> get_upper_bound (b3, i3)
      |> get_upper_bound (b4, i4) in
    mk_range lb ub ~li ~ui
  ;;

  let union_range (a : range) (b : range) : range =
    let lb, li =
      let cmp = compare_bound a.range_lb b.range_lb in
      if cmp < 0
      then a.range_lb, a.range_lb_incl
      else if cmp > 0
      then b.range_lb, b.range_lb_incl
      else a.range_lb, a.range_lb_incl && b.range_lb_incl in
    let ub, ui =
      let cmp = compare_bound a.range_ub b.range_ub in
      if cmp > 0
      then a.range_ub, a.range_ub_incl
      else if cmp < 0
      then b.range_ub, b.range_ub_incl
      else a.range_ub, a.range_ub_incl && b.range_ub_incl in
    mk_range lb ub ~li ~ui
  ;;

  let widen_range (a : range) (b : range) : range =
    let lb, li =
      if compare_bound a.range_lb b.range_lb <= 0
      then a.range_lb, a.range_lb_incl
      else NInf, false in
    let ub, ui =
      if compare_bound a.range_ub b.range_ub >= 0
      then a.range_ub, a.range_ub_incl
      else PInf, false in
    mk_range lb ub ~li ~ui
  ;;

  let combine_range ?(widen = false) (a : range) (b : range) : range =
    if widen
    then
      widen_range a b
      (* union_range a b *)
      (* widen_range a b *)
      (* FIXME: temporarily disable widening *)
    else union_range a b
  ;;

  let add_interval (a : interval) (b : interval) =
    match a, b with
    | Bottom, _ -> Bottom
    | _, Bottom -> Bottom
    | Range ra, Range rb -> Range (add_range ra rb)
  ;;

  let sub_interval (a : interval) (b : interval) =
    match a, b with
    | Bottom, _ -> Bottom
    | _, Bottom -> Bottom
    | Range ra, Range rb -> Range (sub_range ra rb)
  ;;

  let mult_interval (a : interval) (b : interval) =
    match a, b with
    | Bottom, _ -> Bottom
    | _, Bottom -> Bottom
    | Range ra, Range rb -> Range (mult_range ra rb)
  ;;

  let udiv_interval (a : interval) (b : interval) =
    match a, b with
    | Bottom, _ -> Bottom
    | _, Bottom -> Bottom
    | Range ra, Range rb -> Range (udiv_range ra rb)
  ;;

  let intersect_interval (a : interval) (b : interval) : interval =
    match a, b with
    | Bottom, _ -> Bottom
    | _, Bottom -> Bottom
    | Range ra, Range rb ->
      let lb, li =
        let cmp = compare_bound ra.range_lb rb.range_lb in
        if cmp > 0
        then ra.range_lb, ra.range_lb_incl
        else if cmp < 0
        then rb.range_lb, rb.range_lb_incl
        else ra.range_lb, ra.range_lb_incl && rb.range_lb_incl in
      let ub, ui =
        let cmp = compare_bound ra.range_ub rb.range_ub in
        if cmp < 0
        then ra.range_ub, ra.range_ub_incl
        else if cmp > 0
        then rb.range_ub, rb.range_ub_incl
        else ra.range_ub, ra.range_ub_incl && rb.range_ub_incl in
      if compare_bound lb ub > 0
      then Bottom
      else Range (mk_range lb ub ~li ~ui)
  ;;

  let combine_interval ?(widen = false) (a : interval) (b : interval)
      : interval
    =
    match a, b with
    | Bottom, _ -> b
    | _, Bottom -> a
    | Range ra, Range rb -> Range (combine_range ~widen ra rb)
  ;;

  let extract_constant_bound (e : expr) : bound option =
    match e with
    | Int64 i -> Some (Int64 i)
    | _ -> None
  ;;

  let compare_range_ub_int (r : range) (i : int64) : int =
    match r.range_ub with
    | PInf -> 1
    | NInf -> -1
    | Int64 x ->
      if r.range_ub_incl
      then Int64.compare x i
      else Int64.compare (Int64.( - ) x Int64.one) i
    | EInt x ->
      let bi = BInt.of_int64 i in
      let bx = EInt.to_bint x in
      if r.range_ub_incl
      then BInt.compare bx bi
      else BInt.compare (BInt.sub bx BInt.one) bi
    | BInt x ->
      let bi = BInt.of_int64 i in
      if r.range_ub_incl
      then BInt.compare x bi
      else BInt.compare (BInt.sub x BInt.one) bi
  ;;

  let compare_interval_ub_int (itv : interval) (i : int64) : int =
    match itv with
    | Bottom -> -1
    | Range r -> compare_range_ub_int r i
  ;;
end

(*******************************************************************
 ** Core data transfer modules
 *******************************************************************)

module IntervalData = struct
  open IntervalDomain

  (* FIXME: use Map to represent it *)
  type t = (expr, interval) MP.t
end

module RangeUtil = struct
  include IntervalDomain
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

  let equal_data (a : t) (b : t) : bool = MP.equal equal_interval a b

  let lequal_data (a : t) (b : t) : bool =
    MP.for_alli
      ~f:(fun ~key:ea ~data:ia ->
        MP.existsi
          ~f:(fun ~key:eb ~data:ib ->
            equal_expr ea eb && lequal_interval ia ib)
          b)
      a
  ;;

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

  let merge_data ?(widen = false) (a : t) (b : t) : t =
    MP.merge
      ~f:(fun ~key:e i ->
        match i with
        | `Both (ia, ib) -> Some (combine_interval ia ib)
        | `Left ia -> Some ia
        | `Right ib -> Some ib)
      a b
  ;;

  (* FIXME: fix this later *)
  let join_data (a : t) (b : t) : t = merge_data a b

  let clean_irrelevant_info_from_data penv func (d : t) : t =
    MP.filteri
      ~f:(fun ~key:e ~data:i ->
        let vs = collect_llvalue_of_expr e in
        not (List.exists ~f:is_local_llvalue vs))
      d
  ;;

  let clean_info_of_vars (input : t) (vs : llvalues) : t =
    (* TODO: implement later *)
    input
  ;;

  let extract_data_from_predicate ~(widen : bool) (p : predicate) (d : t) : t =
    match p with
    | PBool _ -> MP.empty
    | PIcmp (cmp, lhs, rhs) ->
      let lhs, rhs = expr_of_llvalue lhs, expr_of_llvalue rhs in
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
    let pdata = extract_data_from_predicate ~widen:false p d in
    MP.for_alli
      ~f:(fun ~key:v ~data:vip ->
        let vid = get_interval v d in
        lequal_interval vid vip)
      pdata
  ;;

  let refine_data_by_predicate ?(widen = false) (d : t) (p : predicate) : t =
    (* let refine_interval (a : interval) (b : interval) : interval = *)
    (*   match a, b with *)
    (*   | Bottom, _ -> Bottom *)
    (*   | _, Bottom -> Bottom *)
    (*   | Range ra, Range rb -> *)
    (*     let lb, li = *)
    (*       let cmp = compare_bound ra.range_lb rb.range_lb in *)
    (*       if cmp > 0 *)
    (*       then ra.range_lb, ra.range_lb_incl *)
    (*       else if cmp < 0 *)
    (*       then ra.range_lb, ra.range_lb_incl *)
    (*       else ra.range_lb, ra.range_lb_incl && rb.range_lb_incl in *)
    (*     let ub, ui = *)
    (*       let cmp = compare_bound ra.range_ub rb.range_ub in *)
    (*       if cmp < 0 *)
    (*       then ra.range_ub, ra.range_ub_incl *)
    (*       else if cmp > 0 *)
    (*       then ra.range_ub, ra.range_ub_incl *)
    (*       else ra.range_ub, ra.range_ub_incl && rb.range_ub_incl in *)
    (*     if compare_bound lb ub > 0 *)
    (*     then Bottom *)
    (*     else Range (mk_range lb ub ~li ~ui) in *)
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
      (args : llvalues)
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

  let analyze_instr ?(widen = false) (penv : prog_env) fenv instr (input : t)
      : t
    =
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
    | LO.UDiv  ->
      (* TODO: need to handle Div *)
      let opr0, opr1 = expr_operand instr 0, expr_operand instr 1 in
      let itv0, itv1 = get_interval opr0 input, get_interval opr1 input in
      let itv = udiv_interval itv0 itv1 in
      replace_interval expr itv input
    | LO.SDiv ->
      input
    | LO.PHI ->
      let opr0 = expr_operand instr 0 in
      let itv = ref (get_interval opr0 input) in
      let _ = hdebug "Before refine widening: " pr_bool widen in
      let widen = refine_widening func instr widen in
      let _ = hdebug "After refine widening: " pr_bool widen in
      for i = 1 to num_operands instr - 1 do
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
      if is_type_integer dtyp
      then (
        let bw = LL.integer_bitwidth dtyp in
        (* LLVM integers is represented in two's complement format *)
        let ir = compute_int_max_interval bw in
        replace_interval expr ir input)
      else input
    | LO.Store -> input
    | LO.ZExt ->
      let esrc = expr_of_llvalue (src_of_instr_zext instr) in
      let itv = get_interval esrc input in
      replace_interval expr itv input
    | LO.Call | LO.Invoke ->
      (* TODO: function call for inter-procedural analysis *)
      (* let callee = callee_of_instr_func_call instr in *)
      (* let _ = if is_func_scanf callee then *)
      (*     print "TODO: need to handle func call: scanf" in *)
      input
    | _ -> input
  ;;

  (*******************************************************************
   ** Checking assertions
   *******************************************************************)

  let count_assertions (prog : program) : int =
    (* TODO: implement later if necessary *)
    let assertions =
      List.fold_left
        ~f:(fun acc func -> acc @ AS.find_range_assertions func)
        ~init:[] prog.prog_user_funcs in
    List.length assertions
  ;;

  let check_lower_bound
      (fenv : func_env)
      (instr : instr)
      (v : llvalue)
      (lb : int64)
      : bool
    =
    match get_instr_output fenv instr with
    | None -> false
    | Some data ->
      (match get_interval (expr_of_llvalue v) data with
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

  let check_upper_bound (fenv : func_env) instr (v : llvalue) (ub : int64)
      : bool
    =
    match get_instr_output fenv instr with
    | None -> false
    | Some data ->
      (match get_interval (expr_of_llvalue v) data with
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
      (fenv : func_env)
      instr
      (v : llvalue)
      (lb : int64)
      (ub : int64)
      : bool
    =
    check_lower_bound fenv instr v lb && check_upper_bound fenv instr v ub
  ;;

  let check_assertion (fenvs : func_env list) (ast : AS.assertion)
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

  let check_assertions (penv : prog_env) func : int =
    let assertions = AS.find_range_assertions func in
    let fenvs =
      match Hashtbl.find penv.penv_func_envs func with
      | None -> []
      | Some fenvs -> fenvs in
    let num_checked_assertions = ref 0 in
    let _ =
      List.iter
        ~f:(fun ast ->
          match check_assertion fenvs ast with
          | Some res ->
            let _ = incr num_checked_assertions in
            let _ =
              if res then incr num_valid_asserts else incr num_invalid_asserts
            in
            print_endline (AS.pr_assertion_status func ast res)
          | None -> ())
        assertions in
    !num_checked_assertions
  ;;
end

(*******************************************************************
 ** Main analysis module
 *******************************************************************)

module Analysis = struct
  include RangeTransfer
  include DF.ForwardDataFlow (RangeTransfer)
  module ID = IntervalDomain
  module RU = RangeUtil
  module RT = RangeTransfer

  let get_interval (e : expr) (d : t) : ID.interval = RU.get_interval e d
  let pr_interval (i : ID.interval) = ID.pr_interval i
  let pr_bound (b : ID.bound) : string = ID.pr_bound b

  let pr_interval_concise (i : ID.interval) : string =
    match i with
    | Bottom -> "[Empty]"
    | Range r ->
      if r.ID.range_lb_incl && r.ID.range_ub_incl && r.range_lb == r.range_ub
      then ID.pr_bound r.range_ub
      else ID.pr_range r
  ;;
end
