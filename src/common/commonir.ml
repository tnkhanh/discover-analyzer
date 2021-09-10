(********************************************************************
 * Author: Ta Quang Trung
 * Date: 2020
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore

module LI = Llir
module LL = Llvm
module LO = Llvm.Opcode
module SI = Slir

type program =
  | Llprog of LI.program    (* Llvm program *)
  | Slprog of SI.program

let mk_llvm_prog (prog: LI.program) : program =
  Llprog prog

let mk_seplogic_prog (prog: SI.program) : program =
  Slprog prog
