(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

module LL = Llvm
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LI = Llir
module BG = Bug

(* analyses *)
module PT = Pointer.Analysis
module MS = Memsize.Analysis
module RG = Range.Analysis
module UD = Undef.Analysis

(*******************************************************************
 ** Data structures
 *******************************************************************)

type program_data =
  { pdata_program : LI.program;
    pdata_potential_bugs : BG.potential_bugs;
    pdata_env_memsize : MS.prog_env option;
    pdata_env_pointer : PT.prog_env option;
    pdata_env_range : RG.prog_env option;
    pdata_env_undef : UD.prog_env option
  }

(*******************************************************************
 ** Constructor
 *******************************************************************)

let mk_program_data (prog : LI.program) : program_data =
  { pdata_program = prog;
    pdata_potential_bugs = [];
    pdata_env_memsize = None;
    pdata_env_pointer = None;
    pdata_env_range = None;
    pdata_env_undef = None
  }
;;
