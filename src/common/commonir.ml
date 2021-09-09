(******************************************************************
 ** Author: Ta Quang Trung
 ** Date: 2020
 ******************************************************************)

open Core
open Dcore

module LI = Llir
module LL = Llvm
module LO = Llvm.Opcode
module SI = Slir
module YM = Yaml


type program =
  | Llprog of LI.program    (* Llvm program *)
  | Slprog of SI.program
  | Ymprog of Yaml.value

let mk_llvm_prog (prog: LI.program) : program =
  Llprog prog

let mk_seplogic_prog (prog: SI.program) : program =
  Slprog prog

let mk_yaml_prog (prog: YM.value) : program =
  Ymprog prog
