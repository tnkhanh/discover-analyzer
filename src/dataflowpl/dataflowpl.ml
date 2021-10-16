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
module LV = Llvm.ValueKind
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LP = Llloop
module LG = Llcallgraph
module BG = Bug
module SP = Set.Poly


(*******************************************************************
 ** Core Data and Environment of an analysis
 *******************************************************************)

module type Data = sig

  type t                   (* data representing an abstract program state *)

end


(*******************************************************************
 ** ForwardDataTransfer
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
 ** ForwardDataFlow Analysis
 *******************************************************************)

module ForwardDataFlow (T: ForwardDataTransfer) = struct

  (* sparse analysis *)


  let pr_data d =
    if !print_concise_output && not (is_debug_mode ()) then T.pr_data d
    else T.pr_data d
    (* (pr_data_checksum d) ^ "\n" ^
     * (pr_data d) *)

  let analyze_program prog =
    ()

end
