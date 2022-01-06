(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Llir

type bound =
  | PInf           (* positive infinity *)
  | NInf           (* negative infinity *)
  | Int64 of int64 (* integer 64 bit *)
  | EInt of eint   (* integer in 2-exponential representation *)
  | BInt of bint   (* big integer *)
[@@ocamlformat "disable"]

type range =
  { range_lb : bound; (* lower bound *)
    range_lb_incl : bool; (* lower bound is included *)
    range_ub : bound;
    range_ub_incl : bool
  }

type interval =
  | Bottom
  | Range of range

(* printers *)

let pr_bound (b : bound) : string =
  match b with
  | PInf -> "+inf" (* aa *)
  | NInf -> "-inf"
  | Int64 x -> Int64.to_string x
  | EInt x -> EInt.pr_eint x (* aa *)
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

let mk_interval_int (i : int) : interval =
  let b = Int64 (Int64.of_int i) in
  mk_interval_range ~li:true ~ui:true b b
;;

let mk_interval_int64 (i : int64) : interval =
  let b = Int64 i in
  mk_interval_range ~li:true ~ui:true b b
;;

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
  (* TODO: Division by zero, unsigned div *)
  (* TODO: Handle udiv with negative numbers *)
  match a, b with
  | PInf, PInf -> error "udiv_bound: undefined for PInf / PInf"
  | NInf, _ | _, NInf -> error "udiv_bound: does not work with NInf"
  | PInf, _ -> PInf
  | _, PInf -> Int64 Int64.zero
  (* Int64, BInt, EInt *)
  (* FIXME: need to check y == 0 *)
  | Int64 x, Int64 y -> Int64 (Int64.( / ) x y)
  | Int64 x, EInt y -> EInt (EInt.div_cstyle (EInt.of_int64 x) y)
  | Int64 x, BInt y -> BInt (BInt.div_cstyle (BInt.of_int64 x) y)
  | EInt x, Int64 y -> EInt (EInt.div_cstyle x (EInt.of_int64 y))
  | EInt x, EInt y -> EInt (EInt.div_cstyle x y)
  | EInt x, BInt y -> EInt (EInt.div_cstyle x (EInt.of_bint y))
  | BInt x, Int64 y -> BInt (BInt.div_cstyle x (BInt.of_int64 y))
  | BInt x, EInt y -> EInt (EInt.div_cstyle (EInt.of_bint x) y)
  | BInt x, BInt y -> BInt (BInt.div_cstyle x y)
;;

let sdiv_bound (a : bound) (b : bound) : bound =
  let cmp = compare_bound b (Int64 Int64.zero) in
  if cmp = 0
  then error "sdiv_bound: undefined for _ / 0"
  else (
    match a, b with
    | PInf, PInf -> error "sdiv_bound: undefined for PInf / PInf"
    | PInf, NInf -> error "sdiv_bound: undefined for PInf / NInf"
    | NInf, PInf -> error "sdiv_bound: undefined for NInf / PInf"
    | NInf, NInf -> error "sdiv_bound: undefined for NInf / NInf"
    | _, PInf | _, NInf -> Int64 Int64.zero
    | PInf, _ ->
      if cmp > 0
      then PInf
      else if cmp < 0
      then NInf
      else error "sdiv_bound: undefined for PInf / 0"
    | NInf, _ ->
      let cmp = compare_bound b (Int64 Int64.zero) in
      if cmp > 0
      then NInf
      else if cmp < 0
      then PInf
      else error "sdiv_bound: undefined for NInf / 0"
    (* Int64, BInt, EInt *)
    (* FIXME: need to check y == 0 *)
    | Int64 x, Int64 y -> Int64 (Int64.( / ) x y)
    | Int64 x, EInt y -> EInt (EInt.div_cstyle (EInt.of_int64 x) y)
    | Int64 x, BInt y -> BInt (BInt.div_cstyle (BInt.of_int64 x) y)
    | EInt x, Int64 y -> EInt (EInt.div_cstyle x (EInt.of_int64 y))
    | EInt x, EInt y -> EInt (EInt.div_cstyle x y)
    | EInt x, BInt y -> EInt (EInt.div_cstyle x (EInt.of_bint y))
    | BInt x, Int64 y -> BInt (BInt.div_cstyle x (BInt.of_int64 y))
    | BInt x, EInt y -> EInt (EInt.div_cstyle (EInt.of_bint x) y)
    | BInt x, BInt y -> BInt (BInt.div_cstyle x y))
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
  (* TODO: Add inclusive, Division-by-zero, unsigned ranges are incorrect *)
  let lb = udiv_bound a.range_lb b.range_ub in
  let ub = udiv_bound a.range_ub b.range_ub in
  mk_range lb ub ~li:true ~ui:true
;;

let max_bound (b1 : bound) (b2 : bound) : bound =
  if compare_bound b1 b2 > 0 then b1 else b2
;;

let min_bound (b1 : bound) (b2 : bound) : bound =
  if compare_bound b1 b2 < 0 then b1 else b2
;;

let get_upper_bound (b1, i1) (b2, i2) =
  if compare_bound b1 b2 > 0
  then b1, i1
  else if compare_bound b1 b2 < 0
  then b2, i2
  else b1, i1 || i2
;;

let get_lower_bound (b1, i1) (b2, i2) =
  if compare_bound b1 b2 < 0
  then b1, i1
  else if compare_bound b1 b2 > 0
  then b2, i2
  else b1, i1 || i2
;;

let sdiv_range (a : range) (b : range) : range =
  (* TODO: div_bound can also have 0 *)
  let one = Int64 Int64.one in
  let minus_one = Int64 Int64.minus_one in
  let a_lb = if a.range_lb_incl then a.range_lb else add_bound a.range_lb one in
  let a_ub =
    if a.range_ub_incl then a.range_ub else add_bound a.range_ub minus_one
  in
  let b_lb = if b.range_lb_incl then b.range_lb else add_bound b.range_lb one in
  let b_ub =
    if b.range_ub_incl then b.range_ub else add_bound b.range_ub minus_one
  in
  let bounds_low =
    if compare_bound b_lb (Int64 Int64.zero) = 0
    then []
    else [ sdiv_bound a_lb b_lb; sdiv_bound a_ub b_lb ] in
  let bounds_up =
    if compare_bound b_ub (Int64 Int64.zero) = 0
    then []
    else [ sdiv_bound a_lb b_ub; sdiv_bound a_ub b_ub ] in
  let bounds_one =
    if compare_bound one b_lb >= 0 && compare_bound one b_ub <= 0
    then [ sdiv_bound a_lb one; sdiv_bound a_ub one ]
    else [] in
  let bounds_minus_one =
    if compare_bound minus_one b_lb >= 0 && compare_bound minus_one b_ub <= 0
    then [ sdiv_bound a_lb minus_one; sdiv_bound a_ub minus_one ]
    else [] in
  let all_bounds = bounds_low @ bounds_up @ bounds_one @ bounds_minus_one in
  if List.is_empty all_bounds
  then mk_range PInf NInf
  else (
    let lb = List.fold all_bounds ~init:(List.hd_exn all_bounds) ~f:min_bound in
    let ub = List.fold all_bounds ~init:(List.hd_exn all_bounds) ~f:max_bound in
    mk_range lb ub ~li:true ~ui:true)
;;

let mult_range (a : range) (b : range) =
  let b1, i1 =
    mult_bound a.range_lb b.range_lb, a.range_lb_incl && b.range_lb_incl in
  let b2, i2 =
    mult_bound a.range_lb b.range_ub, a.range_lb_incl && b.range_ub_incl in
  let b3, i3 =
    mult_bound a.range_ub b.range_lb, a.range_ub_incl && b.range_lb_incl in
  let b4, i4 =
    mult_bound a.range_ub b.range_ub, a.range_ub_incl && b.range_ub_incl in
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

let union_interval (a : interval) (b : interval) : interval =
  match a, b with
  | Bottom, _ -> b
  | _, Bottom -> a
  | Range ra, Range rb -> Range (union_range ra rb)
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

let sdiv_interval (a : interval) (b : interval) =
  match a, b with
  | Bottom, _ -> Bottom
  | _, Bottom -> Bottom
  | Range ra, Range rb ->
    let res = sdiv_range ra rb in
    (match res.range_lb, res.range_ub with
    | PInf, NInf -> Bottom
    | _ -> Range res)
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
    if compare_bound lb ub > 0 then Bottom else Range (mk_range lb ub ~li ~ui)
;;

let combine_interval ?(widen = false) (a : interval) (b : interval) : interval =
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

let compare_interval_ub_int (itv : interval) (i : int) : int =
  match itv with
  | Bottom -> -1
  | Range r -> compare_range_ub_int r (Int64.of_int i)
;;

let compare_interval_ub_int64 (itv : interval) (i : int64) : int =
  match itv with
  | Bottom -> -1
  | Range r -> compare_range_ub_int r i
;;

let compare_interval_ub_bound (itv : interval) (b : bound) : int =
  match itv with
  | Bottom -> -1
  | Range r -> compare_bound r.range_ub b
;;
