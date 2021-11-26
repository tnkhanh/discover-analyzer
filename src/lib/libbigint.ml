(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

module BInt = struct
  include Big_int

  let one = Big_int.unit_big_int
  let zero = Big_int.zero_big_int
  let sub = Big_int.sub_big_int
  let add = Big_int.add_big_int
  let mult = Big_int.mult_big_int
  let div = Big_int.div_big_int
  let neg = Big_int.minus_big_int

  let compute_lower_bound_two_complement (bitwidth : int) : big_int =
    let x = Big_int.power_int_positive_int 2 (bitwidth - 1) in
    neg x
  ;;

  let compute_upper_bound_two_complement (bitwidth : int) : big_int =
    let x = Big_int.power_int_positive_int 2 (bitwidth - 1) in
    sub x one
  ;;

  (** return lower bound, upper bound of a n bits two's complement number *)
  let compute_range_two_complement (bitwidth : int) : big_int * big_int =
    let x = Big_int.power_int_positive_int 2 (bitwidth - 1) in
    let lb = neg x in
    let ub = sub x one in
    lb, ub
  ;;

  let pr_bigint = Big_int.string_of_big_int
end
