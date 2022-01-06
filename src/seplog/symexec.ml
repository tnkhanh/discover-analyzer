(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module LI = Llir
module PV = Prover
module SA = Slast
module SI = Slir
module TF = Transform
module TI = Typeinfer

(* TODO: implement seplog result *)
type symexec_result =
  { symexec_analysis_time : float;
    symexec_total_time : float
  }

let parse_program_seplog (filename : string) : SA.program =
  let pr_position fname lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    let line = pos.Lexing.pos_lnum in
    let col = pos.Lexing.pos_cnum - pos.Lexing.pos_bol + 1 in
    "filename: " ^ fname ^ ", line: " ^ pr_int line ^ ", col: " ^ pr_int col
  in
  let inchan =
    try open_in filename
    with e -> error ("Unable to open file:\n " ^ filename) in
  let lexbuf = Lexing.from_channel inchan in
  let prog =
    try Parser.program Lexer.read lexbuf with
    | Lexer.SyntaxError msg ->
      error
        (("Syntax error: " ^ msg ^ "\n")
        ^ "Location: "
        ^ pr_position filename lexbuf
        ^ "\n")
    | Parser.Error ->
      error
        ("Syntax error!\n" ^ "Location: " ^ pr_position filename lexbuf ^ "\n")
    | End_of_file -> error ("Unable to parse filename: " ^ filename) in
  let _ = close_in inchan in
  prog
;;

let process_command (prog : SI.program) (cmd : SI.command) : unit =
  match cmd with
  | SI.CheckSat f ->
    print_endline ("\n[+] CheckSat: " ^ SI.pr_f f);
    (* let res = check_sat prog [f] in *)
    print_endline ("\n ==> Result: need to implement" ^ "\n")
  | SI.ProveEntails ents ->
    print_endline ("\n[+] ProveEntails:\n" ^ SI.pr_ents ents);
    let res = PV.prove_entailments prog ents in
    print_endline ("\n ==> Result: " ^ pr_bool_option res ^ "\n")
  | SI.InferFrame ent ->
    print_endline ("\n[+] InferFrame: " ^ SI.pr_ent ent);
    let res, frame = PV.infer_entailment_frame prog ent in
    print_endline ("\n ==> Result: " ^ pr_bool_option res);
    print_endline ("\n ==> Frame: " ^ SI.pr_fs frame ^ "\n")
;;

let compile_lib_seplog () : unit =
  match !Verifier.lib_core with
  | Some _ -> ()
  | None ->
    let ilib = parse_program_seplog !lib_core_file in
    let _ =
      debug ~ruler:`Medium ~enable:!print_input_prog
        ("INPUT LIBS:\n\n" ^ SA.pr_program ilib ^ "\n\n") in
    let ilib = TI.infer_typ_program ilib in
    let _ =
      debug ~ruler:`Medium
        ~enable:(!print_input_prog && !print_type)
        ("TYPED LIBS:\n\n" ^ SA.pr_program ilib ^ "\n\n") in
    let clib = TF.transform_program ilib in
    let _ =
      debug ~ruler:`Medium ~enable:!print_core_prog
        ("CORE LIBS:\n\n" ^ SI.pr_program clib ^ "\n\n") in
    ignore (Verifier.lib_core := Some clib)
;;

let compile_sep_logic (filename : string) : SI.program =
  let iprog = parse_program_seplog filename in
  let _ =
    debug ~ruler:`Medium ~enable:!print_input_prog
      ("INPUT PROGRAMS:\n\n" ^ SA.pr_program iprog ^ "\n\n") in
  let iprog = TI.infer_typ_program iprog in
  let _ =
    debug ~ruler:`Medium
      ~enable:(!print_input_prog && !print_type)
      ("TYPED PROGRAMS:\n\n" ^ SA.pr_program iprog ^ "\n\n") in
  let cprog = TF.transform_program iprog in
  let _ =
    debug ~ruler:`Medium ~enable:!print_core_prog
      ("CORE PROGRAMS:\n\n" ^ SI.pr_program cprog ^ "\n\n") in
  cprog
;;

(* TODO: rename to a better function name *)
let compute_analysis_result () : symexec_result =
  let total_time = ref 0. in
  let analysis_time = ref 0. in
  (* FIXME: compute run time correctly *)
  { symexec_total_time = !total_time; symexec_analysis_time = !analysis_time }
;;

let verify_program (prog : SI.program) : symexec_result =
  let _ = debug (SI.pr_program prog) in
  let _ = debug "\n===================================\n" in
  let _ = List.iter ~f:(process_command prog) prog.prog_commands in
  compute_analysis_result ()

;;

let analyze_program (prog : LI.program) : symexec_result =
  let _ = print "Analyze program by Separation Logic" in
  let _ = compile_lib_seplog () in
  let _ = Verifier.verify_program prog in
  compute_analysis_result ()
;;
