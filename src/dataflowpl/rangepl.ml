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
module DF = Dataflowpl
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

  type t2 = (expr, interval) MP.t

  (* type t = einterval list         (\* sorted by expr *\) *)

end

module RangeTransfer : (DF.ForwardDataTransfer with type t = IntervalData.t) = struct
  open IntervalDomain

  type t = IntervalData.t

  type global_env = {
    genv_global_output : (global, t) Hashtbl.t;
    mutable genv_globals_data : t;
  }

  type prog_env = {
    penv_prog : program;
    penv_global_env : global_env;
  }

  let get_global_output (genv: global_env) global : t option =
    Hashtbl.find genv.genv_global_output global

  let set_global_output (genv: global_env) global (data: t) : unit =
    Hashtbl.set genv.genv_global_output ~key:global ~data

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

  let foo () =
    ()

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

  (* let a = RT.foo () *)

  let get_interval (e: expr) (d: RT.t) : ID.interval =
    match e with
    | Int64 i -> ID.interval_of_bound (Int64 i)
    | _ ->
      match MP.find d e with
      | Some i -> i
      | None -> IntervalDomain.least_interval


end
