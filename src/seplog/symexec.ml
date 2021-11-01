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


module AG = Arguments
module BG = Bug
module BR = Llvm_bitreader
module CI = Commonir
module DF = Dataflow
module LI = Llir
module LL = Llvm
module NO = Normalize
module PS = Process
module PV = Prover
module SA = Slast
module SI = Slir
module TF = Transform
module TI = Typeinfer


let parse_program_seplog (filename: string) : SA.program =
  let pr_position fname lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    let line = pos.Lexing.pos_lnum in
    let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
    "filename: " ^ fname ^ ", line: " ^ (pr_int line) ^
    ", col: " ^ (pr_int col) in
  let inchan =
    try open_in filename
    with e -> error ("Unable to open file:\n " ^ filename) in
  let lexbuf = Lexing.from_channel inchan in
  let prog =
    try Parser.program Lexer.read lexbuf
    with
    | Lexer.SyntaxError msg ->
      error ("Syntax error: " ^ msg ^ "\n" ^
             "Location: " ^ (pr_position filename lexbuf) ^ "\n")
    | Parser.Error ->
      error ("Syntax error!\n" ^
             "Location: " ^ (pr_position filename lexbuf) ^ "\n")
    | End_of_file -> error ("Unable to parse filename: " ^ filename) in
  let _ = close_in inchan in
  prog


let process_command (prog: SI.program) (cmd: SI.command) : unit =
  match cmd with
  | SI.CheckSat f ->
    print_endline ("\n[+] CheckSat: " ^ (SI.pr_f f));
    (* let res = check_sat prog [f] in *)
    print_endline ("\n ==> Result: need to implement" ^ "\n")
  | SI.ProveEntails ents ->
    print_endline ("\n[+] ProveEntails:\n" ^ (SI.pr_ents ents));
    let res = PV.prove_entailments prog ents in
    print_endline ("\n ==> Result: " ^ (pr_bresult res) ^ "\n")
  | SI.InferFrame ent ->
    print_endline ("\n[+] InferFrame: " ^ (SI.pr_ent ent));
    let res, frame = PV.infer_entailment_frame prog ent in
    print_endline ("\n ==> Result: " ^ (pr_bresult res));
    print_endline ("\n ==> Frame: " ^ (SI.pr_fs frame) ^ "\n")


let compile_lib_seplog () =
  let ilib = parse_program_seplog !lib_core_file in
  let _ = if !print_input_prog then (
    debugc ("=====================================\n" ^
            "INPUT LIBS:\n\n" ^ (SA.pr_program ilib) ^ "\n\n")) in
  let ilib = TI.infer_typ_program ilib in
  let _ = if !print_input_prog && !print_type then (
    debugc ("=====================================\n" ^
            "TYPED LIBS:\n\n" ^ (SA.pr_program ilib) ^ "\n\n")) in
  let clib = TF.transform_program ilib in
  let _ = if !print_core_prog then (
    debugc ("=====================================\n" ^
            "CORE LIBS:\n\n" ^ (SI.pr_program clib) ^ "\n\n")) in
  clib


let compile_sep_logic (filename: string) : (SI.program) =
  let iprog = parse_program_seplog filename in
  let _ = if !print_input_prog then (
      debugc ("=====================================\n" ^
              "INPUT PROGRAMS:\n\n" ^ (SA.pr_program iprog) ^ "\n\n")) in
  let iprog = TI.infer_typ_program iprog in
  let _ = if !print_input_prog && !print_type then (
      debugc ("=====================================\n" ^
              "TYPED PROGRAMS:\n\n" ^ (SA.pr_program iprog) ^ "\n\n")) in
  let cprog = TF.transform_program iprog in
  let _ = if !print_core_prog then (
      debugc ("=====================================\n" ^
              "CORE PROGRAMS:\n\n" ^ (SI.pr_program cprog) ^ "\n\n")) in
  cprog


let analyze_program_seplog (prog: SI.program) : unit =
  debugc ((SI.pr_program prog) ^
          "\n\n===================================\n");
  List.iter ~f:(process_command prog) prog.prog_commands


let analyze_program_llvm (prog: LI.program) : unit =
  let _ = print "Analyze program by Separation Logic" in
  let _ = Verifier.lib_core := compile_lib_seplog () in
  Verifier.verify_program prog
