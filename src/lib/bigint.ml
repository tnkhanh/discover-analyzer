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
  let subtract = Big_int.sub_big_int
  let neg = Big_int.minus_big_int

  let compute_lower_bound_two_complement (n : int) : big_int =
    let x = Big_int.power_int_positive_int 2 (n - 1) in
    neg x
  ;;

  let compute_upper_bound_two_complement (n : int) : big_int =
    let x = Big_int.power_int_positive_int 2 (n - 1) in
    subtract x one
  ;;

  (** return lower bound, upper bound of a n bits two's complement number *)
  let compute_range_two_complement (n : int) : big_int * big_int =
    let x = Big_int.power_int_positive_int 2 (n - 1) in
    let lb = neg x in
    let ub = subtract x one in
    lb, ub
  ;;

  let sprint_bigint = Big_int.string_of_big_int
end
