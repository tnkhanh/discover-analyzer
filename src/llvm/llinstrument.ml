(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Globals
open Lib
open Sprinter
open Printer
open Debugger

open Llir

module LL = Llvm
module LO = Llvm.Opcode
module LD = Llvm_debuginfo
module LS = Lldebug
module LV = Llvm.ValueKind
module SP = Set.Poly
module LP = Llloop
module LG = Llcallgraph


let instrument_bug_annotation (modul: LL.llmodule) : unit =
  (* TODO: fill code here.
     See module llsimp.ml, function elim_instr_intrinsic_lifetime ...
     for how to manipulating LLVM bitcode *)
  let _ = print "INSTRUMENT BUG ANNOTATION" in
  ()


let instrument_bitcode filename (modul: LL.llmodule) : unit =
  instrument_bug_annotation modul
