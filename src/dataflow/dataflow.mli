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
module LL = Llvm
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LP = Llloop
module BG = Bug
module SP = Set.Poly


(*******************************************************************
 ** Core Data
 *******************************************************************)

module type Data = sig
  type t                   (* data representing an abstract program state *)
end

(*******************************************************************
 ** Data Environment
 *******************************************************************)

module type DataUtil = sig

  type t

  type callsite = {
    cs_instr_call : instr;
    cs_caller : func;
    cs_caller_input : t;
    cs_caller_callsites : callsite list;
    cs_callee : func;
    cs_callee_input : t;
  }

  type exn = {
    exn_orig_expr : expr;
    exn_root_expr : expr;
    exn_data : t;
    exn_type_info : llvalue;
  }

  type exns = exn list

  type working_block = {
    wb_block: block;
    wb_instr: instr;
    wb_instr_input : t;
  }

  type working_func = {
    wf_callsites: callsite list;
    wf_func: func;
    wf_input: t;
  }

  type global_env = {
    genv_global_output : (global, t) Base.Hashtbl.t;
    mutable genv_globals_data : t;
  }

  type func_env = {
    fenv_id : string;
    fenv_func : func;
    fenv_callsites : callsite list;
    fenv_prog : program;
    fenv_instr_output : (instr, t) Base.Hashtbl.t;
    fenv_block_input : (block, t) Base.Hashtbl.t;
    mutable fenv_input : t;
    mutable fenv_output : t option;
    fenv_thrown_exn : (llvalue, exn) Hashtbl.t;
    fenv_landing_exns : (llvalue, exn list) Hashtbl.t;
    mutable fenv_deref_params : params;
    mutable fenv_deref_globals : globals;
    mutable fenv_working_blocks : working_block list;
    mutable fenv_state_changed : bool;
  }

  type func_summary = {
    fsum_func : func;
    fsum_input : t;
    fsum_output : t;
    fsum_thrown_exn : exn list;
    fsum_deref_params : params;
    fsum_deref_globals : globals;
  }

  type prog_env = {
    penv_prog : program;
    penv_global_env : global_env;
    penv_func_envs : (func, func_env list) Hashtbl.t;
    penv_func_summaries : (func, func_summary list) Base.Hashtbl.t;
    penv_func_analyzed_inputs : (func, t list) Base.Hashtbl.t;
    mutable penv_goal_funcs : funcs;
    mutable penv_working_funcs : working_func list;
    penv_sparse_llvalue : (llvalue, bool) Hashtbl.t;
    penv_sparse_block : (block, bool) Hashtbl.t;
    penv_sparse_func : (func, bool) Hashtbl.t;
    penv_sparse_used_globals: (func, globals) Hashtbl.t;
    penv_block_sparse_instrs : (block, instr list) Hashtbl.t;
    penv_sparse_precedings_block : (block, block list) Hashtbl.t;
    penv_sparse_succeedings_block : (block, block list) Hashtbl.t;
    penv_sparse_reachable_blocks : (block, block list) Hashtbl.t;
    penv_func_analyzed_times : (func, int) Base.Hashtbl.t;
    penv_block_local_analyzed_times : (block, int) Base.Hashtbl.t;
    penv_block_total_analyzed_times : (block, int) Base.Hashtbl.t;
    penv_func_analysis_stack : func Stack.t;
    penv_block_analyzed_squence : (func, blocks) Hashtbl.t;
  }

  val get_global_output : global_env -> global -> t option
  val set_global_output : global_env -> global -> t -> unit

  val get_instr_output : func_env -> instr -> t option
  val set_instr_output : func_env -> instr -> t -> unit

  val get_block_input : func_env -> block -> t option
  val set_block_input : func_env -> block -> t -> unit

  val init_sparse_globals_instrs : prog_env -> unit
  val refine_sparse_globals_instrs : prog_env -> bool

  val pre_analyze_func : prog_env -> func_env -> unit
  val post_analyze_func : prog_env -> func_env -> unit

  val pre_analyze_prog : prog_env -> unit
  val post_analyze_prog : prog_env -> unit

  val is_sparse_llvalue : prog_env -> llvalue -> bool
  val is_sparse_global : prog_env -> global -> bool
  val is_sparse_instr : prog_env -> instr -> bool
  val is_sparse_block : prog_env -> block -> bool
  val is_sparse_func : prog_env -> func -> bool
  val get_sparse_used_globals : prog_env -> func -> globals

end

(*******************************************************************
 ** Default Data Environment Generator
 *******************************************************************)

module DataUtilGenerator : functor (M: Data) -> sig

  include (DataUtil with type t := M.t)

end


(*******************************************************************
 ** Forward Data Transfer
 *******************************************************************)

module type ForwardDataTransfer = sig

  include Data
  include DataUtil

  val analysis : dfa_analysis

  val pr_data : t -> string
  val pr_data_checksum : t -> string
  val least_data : t
  val equal_data : t -> t -> bool
  val lequal_data : t -> t -> bool
  val copy_data : t -> t
  val subst_data : ?sstv:substv -> ?sstve:substve -> ?sste:subste -> t -> t
  val merge_data : ?widen:bool -> t -> t -> t
  val join_data : t -> t -> t
  val need_widening : func -> bool
  val clean_irrelevant_info_from_data : prog_env -> func -> t -> t
  val clean_info_of_vars : t -> llvalues -> t
  val is_data_satisfied_predicate : t -> predicate -> bool
  val refine_data_by_predicate : ?widen:bool -> t -> predicate -> t
  val prepare_callee_input :
    prog_env -> instr -> func -> llvalue list -> t -> t
  val compute_callee_output_exns :
    prog_env -> instr -> func -> llvalue list -> t -> func_summary -> t * exns
  val prepare_thrown_exception_data : prog_env -> llvalue -> llvalue -> t -> t
  val compute_catch_exception_data : prog_env -> instr -> llvalue -> t -> exn -> t
  val analyze_global : global -> t -> t
  val analyze_instr : ?widen:bool -> prog_env -> func_env -> instr -> t -> t
  (* val pre_analyze_prog : prog_env -> unit
   * val post_analyze_prog : prog_env -> unit *)
  val check_bug : func_env -> BG.bug -> ternary
  val count_assertions : program -> int
  val check_assertions : prog_env -> func -> int
end


(*******************************************************************
 ** Forward Data-flow Analysis
 *******************************************************************)

module ForwardDataFlow : functor (T: ForwardDataTransfer) -> sig
  val pr_prog_env : T.prog_env -> string
  val is_intra_proc_dfa_mode : unit -> bool
  val get_func_analyzed_times : T.prog_env -> func -> int
  val is_call_to_user_function : instr -> bool
  val is_call_to_func_pointer : instr -> bool
  val analyze_functions : T.prog_env -> unit
  val analyze_globals : T.prog_env -> unit
  val analyze_program_intraproc : ?func: func option -> T.prog_env -> T.prog_env
  val analyze_program_interproc : ?func: func option -> T.prog_env -> T.prog_env
  val analyze_program : ?interproc:bool -> program -> T.prog_env
  val check_bug : T.prog_env -> BG.bug -> ternary
  val check_assertions : T.prog_env -> unit
  val report_analysis_stats : T.prog_env -> unit
end
