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
 ** Forward Data Transfer
 *******************************************************************)

module type ForwardDataTransfer = sig

  type t

  type global_env = {
    genv_global_output : (global, t) Base.Hashtbl.t;
    mutable genv_globals_data : t;
  }

  type prog_env = {
    penv_prog : program;
    penv_global_env : global_env;
  }

  val get_global_output : global_env -> global -> t option
  val set_global_output : global_env -> global -> t -> unit

  (* include (DataUtil with type t := D.t) *)

  val analysis : dfa_analysis

  val pr_data : t -> string

  (* val foo : unit -> unit *)

end


(*******************************************************************
 ** Forward Data-flow Analysis
 *******************************************************************)

module ForwardDataFlow : functor (T: ForwardDataTransfer) -> sig
  (* val pr_prog_env : D.prog_env -> string *)
  (* val is_intra_proc_dfa_mode : unit -> bool *)
  (* val get_func_analyzed_times : D.prog_env -> func -> int *)
  (* val is_call_to_user_function : instr -> bool *)
  (* val is_call_to_func_pointer : instr -> bool *)
  (* val analyze_functions : D.prog_env -> unit *)
  (* val analyze_globals : D.prog_env -> unit *)
  (* val analyze_program_intraproc : ?func: func option -> D.prog_env -> D.prog_env *)
  (* val analyze_program_interproc : ?func: func option -> D.prog_env -> D.prog_env *)
  (* val analyze_program : ?interproc:bool -> program -> D.prog_env *)
  (* val check_bug : D.prog_env -> BG.bug -> ternary *)
  (* val check_assertions : D.prog_env -> unit *)
  (* val report_analysis_stats : D.prog_env -> unit *)

  val analyze_program : program -> unit
end
