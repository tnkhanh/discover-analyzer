(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

type integer_overflow =
  { iof_expr : Llir.llvalue;
    iof_bitwidth : int;
    iof_instr : Llir.instr
  }

type integer_underflow =
  { iuf_expr : Llir.llvalue;
    iuf_bitwidth : int;
    iuf_instr : Llir.instr
  }

type integer_coercion_error =
  { ice_expr : Llir.llvalue;
    ice_instr : Llir.instr
  }

type numeric_truncation_error =
  { nte_expr : Llir.llvalue;
    nte_instr : Llir.instr
  }

type division_by_zero =
  { dbz_expr : Llir.llvalue;
    dbz_instr : Llir.instr
  }

type memory_leak =
  { mlk_pointer : Llir.llvalue;
    mlk_size : int option
  }

type null_pointer_deref = { npe_pointer : Llir.llvalue }

type buffer_size =
  | NumElem of (int64 * Llir.lltype)
  | MemSizeOf of Llir.llvalue

type buffer_overflow =
  { bof_pointer : Llir.llvalue;
    bof_elem_index : Llir.llvalue;
    bof_buff_size : buffer_size;
    bof_write_operation : bool;
    bof_stack_based : bool;
    bof_instr : Llir.instr
  }

type resource_leak =
  { rlk_pointer : Llir.llvalue;
    rlk_file_resource : bool
  }

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

val pr_instr_location_and_code_excerpt : Llir.instr -> string
val pr_buffer_overflow_info : buffer_overflow -> string
val pr_memory_leak_info : memory_leak -> string
val pr_llvalue_name : Llvm.llvalue -> string
val pr_integer_overflow_info : ?detailed:bool -> integer_overflow -> string
val pr_integer_underflow_info : ?detailed:bool -> integer_underflow -> string
val pr_bug_cwe : bug_type -> string
val pr_bug_type : ?detailed:bool -> bug_type -> string
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
val mk_potential_integer_overflow : Llir.instr -> potential_bug
val mk_potential_integer_underflow : Llir.instr -> potential_bug
val mk_potential_buffer_overflow : Llir.instr -> potential_bug
val mk_potential_memory_leak : Llir.instr -> potential_bug
val mk_real_bug : reason:string -> checker:string -> potential_bug -> bug

(*** queries ***)
val is_bug_buffer_overflow : bug -> bool
val is_bug_memory_leak : bug -> bool
val is_bug_integer_overflow : bug -> bool
val is_bug_integer_underflow : bug -> bool

(*** annotating and reporting ***)

val report_bug_stats : bug list -> unit
val annotate_potential_bugs : Llir.program -> potential_bugs
