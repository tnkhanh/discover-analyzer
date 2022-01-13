(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

module ArithmeticBug : sig
  type integer_overflow =
    { iof_expr : Llir.value;
      iof_bitwidth : int;
      iof_instr : Llir.instr
    }

  type integer_underflow =
    { iuf_expr : Llir.value;
      iuf_bitwidth : int;
      iuf_instr : Llir.instr
    }

  type integer_coercion_error =
    { ice_expr : Llir.value;
      ice_instr : Llir.instr
    }

  type numeric_truncation_error =
    { nte_expr : Llir.value;
      nte_instr : Llir.instr
    }

  type division_by_zero =
    { dbz_expr : Llir.value;
      dbz_instr : Llir.instr
    }
end

module MemoryBug : sig
  type memory_leak =
    { mlk_pointer : Llir.value;
      mlk_size : int option
    }

  type null_pointer_deref = { npe_pointer : Llir.value }

  type buffer_overflow =
    { bof_pointer : Llir.value;
      bof_elem_index : Llir.value;
      bof_instr : Llir.instr;
      mutable bof_write_operation : bool option;
      mutable bof_stack_based : bool option
    }
end

module ResourceBug : sig
  type resource_leak =
    { rlk_pointer : Llir.value;
      rlk_file_resource : bool
    }
end

module SolidityBug : sig
  type solidity_access_control = { sac_pointer : Llir.value }
end

include module type of ArithmeticBug
include module type of MemoryBug
include module type of ResourceBug
include module type of SolidityBug

type bug_type =
  | IntegerOverflow of integer_overflow option
  | IntegerUnderflow of integer_underflow option
  | IntegerCoercionError of integer_coercion_error option
  | NumericTruncationError of numeric_truncation_error option
  | DivisionByZero of division_by_zero option
  | MemoryLeak of memory_leak option
  | NullPointerDeref of null_pointer_deref option
  | BufferOverflow of buffer_overflow option
  | ResourceLeak of resource_leak option
  | SolidityAccessControl of solidity_access_control

type bug_types = bug_type list

type potential_bug =
  { pbug_instr : Llir.instr;
    pbug_func : Llir.func;
    pbug_type : bug_type
  }

type potential_bugs = potential_bug list

type bug =
  { bug_instr : Llir.instr;
    bug_func : Llir.func;
    bug_type : bug_type;
    bug_checker : string;
    bug_reason : string
  }

type bugs = bug list

(*** printing ***)

val pr_bug_type : bug_type -> string
val pr_potential_bug : potential_bug -> string
val pr_potential_bugs : potential_bug list -> string
val pr_bug : bug -> string
val pr_bug_name : bug -> string
val pr_bugs : bug list -> string

(*** constructor ***)

val mk_bug_type_memory_leak : unit -> bug_type
val mk_bug_type_null_pointer_deref : unit -> bug_type
val mk_bug_type_buffer_overflow_deref : unit -> bug_type
val mk_bug_type_integer_overflow : unit -> bug_type
val mk_bug_type_integer_underflow : unit -> bug_type
val mk_bug_type_coercion_error : unit -> bug_type
val mk_bug_type_truncation_error : unit -> bug_type
val mk_bug_type_division_by_zero : unit -> bug_type
val mk_potential_bug : Llir.instr -> bug_type -> potential_bug
val mk_real_bug : reason:string -> checker:string -> potential_bug -> bug

(*** queries ***)
val is_bug_buffer_overflow : bug -> bool
val is_bug_memory_leak : bug -> bool
val is_bug_integer_overflow : bug -> bool
val is_bug_integer_underflow : bug -> bool

(*** reporting ***)

val report_bug_stats : bug list -> unit
val mark_potential_bugs : Llir.program -> potential_bugs
