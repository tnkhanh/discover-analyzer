(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core

type bint = Big_int.big_int

(*******************************************************************
 * Big integer (concise wrapper of Big_int)
 *******************************************************************)

module BInt = struct
  (*** printing ***)

  let pr_bint = Big_int.string_of_big_int

  (*** comparisons ***)

  let eq = Big_int.eq_big_int
  let gt = Big_int.gt_big_int
  let ge = Big_int.ge_big_int
  let lt = Big_int.lt_big_int
  let le = Big_int.le_big_int
  let compare = Big_int.compare_big_int

  (*** conversions ***)

  let of_int = Big_int.big_int_of_int
  let of_int32 = Big_int.big_int_of_int32
  let of_int64 = Big_int.big_int_of_int64

  (*** constants ***)

  let one = Big_int.unit_big_int
  let minus_one = Big_int.minus_big_int Big_int.unit_big_int
  let zero = Big_int.zero_big_int

  (*** arithmetic computations ***)

  let sub = Big_int.sub_big_int
  let add = Big_int.add_big_int
  let neg = Big_int.minus_big_int
  let mult = Big_int.mult_big_int
  let mult_int_bint = Big_int.mult_int_big_int
  let div = Big_int.div_big_int
  let pow_int_positive_int = Big_int.power_int_positive_int

  (*** two complement number ***)

  let compute_lower_bound_two_complement (bitwidth : int) : bint =
    let x = pow_int_positive_int 2 (bitwidth - 1) in
    neg x
  ;;

  let compute_upper_bound_two_complement (bitwidth : int) : bint =
    let x = pow_int_positive_int 2 (bitwidth - 1) in
    sub x one
  ;;

  (** return lower bound, upper bound of a n bits two's complement number *)
  let compute_range_two_complement (bitwidth : int) : bint * bint =
    let x = pow_int_positive_int 2 (bitwidth - 1) in
    let lb = neg x in
    let ub = sub x one in
    lb, ub
  ;;
end

(*******************************************************************
 * Exponent-of-2 representation of integer
 *******************************************************************)

module EInt = struct
  (*** type declarations ***)

  (** List of coefficients and exponents of 2 (sorted by the exponents)
      and the constant factor. For example:
        ([(a, 5); (b, 1)], c) = a * 2^5 + b * 2^1 + c *)
  type eint = (bint * int) list * bint

  (*** printing  ***)

  let pr_eint (x : eint) : string =
    let ces, c = x in
    let res = BInt.pr_bint c in
    match ces with
    | [] -> res
    | _ ->
      List.fold_left
        ~f:(fun acc (c, e) -> BInt.pr_bint c ^ " * 2^" ^ string_of_int e ^ acc)
        ~init:(" + " ^ res) ces
  ;;

  (*** constant  ***)

  let one : eint = [], BInt.one
  let zero : eint = [], BInt.zero

  (*** conversions  ***)

  let of_int (x : int) : eint = [], BInt.of_int x
  let of_int64 (x : int64) : eint = [], BInt.of_int64 x
  let of_bint (x : bint) : eint = [], x

  let to_bint (x : eint) : bint =
    let ces, c = x in
    let res = c in
    List.fold_left
      ~f:(fun acc (c, e) ->
        BInt.add acc (BInt.mult c (BInt.pow_int_positive_int 2 e)))
      ~init:res ces
  ;;

  (*** arithmetic operations ***)

  (** Add two lists of coefficients and exponents.
      The lists of coefficients and exponents are sorted by exponents. *)
  let add_coeffients_exponents
      (ces1 : (bint * int) list)
      (ces2 : (bint * int) list)
      : (bint * int) list
    =
    let rec add_ces ces1 ces2 acc =
      match ces1, ces2 with
      | [], _ -> acc @ ces2
      | _, [] -> acc @ ces1
      | (c1, e1) :: nces1, (c2, e2) :: nces2 ->
        if e1 > e2
        then add_ces nces1 ces2 (acc @ [ c1, e1 ])
        else if e1 < e2
        then add_ces ces1 nces2 (acc @ [ c2, e2 ])
        else add_ces nces1 nces2 (acc @ [ BInt.add c1 c2, e1 ]) in
    add_ces ces1 ces2 []
  ;;

  let add (x : eint) (y : eint) : eint =
    let ces1, c1 = x in
    let ces2, c2 = y in
    add_coeffients_exponents ces1 ces2, BInt.add c1 c2
  ;;

  let neg (x : eint) : eint =
    let ces, c = x in
    let nces = List.map ~f:(fun (c, e) -> BInt.neg c, e) ces in
    let nc = BInt.neg c in
    nces, nc
  ;;

  let sub (x : eint) (y : eint) : eint = add x (neg y)

  let mult_int_eint (x : int) (y : eint) : eint =
    let ces, c = y in
    let nces = List.map ~f:(fun (c, e) -> BInt.mult_int_bint x c, e) ces in
    let nc = BInt.mult_int_bint x c in
    nces, nc
  ;;

  let mult_bint_eint (x : bint) (y : eint) : eint =
    let ces, c = y in
    let nces = List.map ~f:(fun (c, e) -> BInt.mult x c, e) ces in
    let nc = BInt.mult x c in
    nces, nc
  ;;

  let mult (x : eint) (y : eint) : eint =
    let ces1, c1 = x in
    let ces2, c2 = y in
    let res = [], BInt.mult c1 c2 in
    let res = add res (mult_bint_eint c1 (ces2, BInt.zero)) in
    let res = add res (mult_bint_eint c2 (ces1, BInt.zero)) in
    List.fold_left
      ~f:(fun acc1 (c1, e1) ->
        List.fold_left
          ~f:(fun acc2 (c2, e2) ->
            let u = [ BInt.mult c1 c2, e1 + e2 ], BInt.zero in
            add acc2 u)
          ~init:acc1 ces1)
      ~init:res ces2
  ;;

  let div (x : eint) (y : eint) : eint =
    let bx, by = to_bint x, to_bint y in
    let c = BInt.div bx by in
    let res = [], c in
    res
  ;;

  (*** two complement number ***)

  let compute_lower_bound_two_complement (bitwidth : int) : eint =
    [ BInt.minus_one, bitwidth - 1 ], BInt.zero
  ;;

  (* let x = pow_int_positive_int 2 (bitwidth - 1) in *)
  (* neg x *)

  let compute_upper_bound_two_complement (bitwidth : int) : eint =
    [ BInt.one, bitwidth - 1 ], BInt.minus_one
  ;;

  (* let x = pow_int_positive_int 2 (bitwidth - 1) in *)
  (* sub x one *)

  (** return lower bound, upper bound of a n bits two's complement number *)
  let compute_range_two_complement (bitwidth : int) : eint * eint =
    let lb = compute_lower_bound_two_complement bitwidth in
    let ub = compute_upper_bound_two_complement bitwidth in
    lb, ub
  ;;
end

type eint = EInt.eint
