(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore
open Llir

module LL = Llvm
module LO = Llvm.Opcode
module LD = Llvm_debuginfo
module LS = Llsrc
module LV = Llvm.ValueKind
module SP = Set.Poly
module LP = Llloop
module LG = Llcfg
module FM = Map.Make(String)
module AP = Annparser

type annotation =
  | Bug of position
  | Safe of position

type instr_with_tag = {
  line: int;
  col: int;
  tag: int;
  ins: instr;
}

let make_tagged_ins line_ col_ tag_ ins_= 
  { line = line_;
    col = col_;
    tag = tag_;
    ins = ins_;
  }

let extract_ann_marks (filename: string) = 
(*  let _ = print_endline ("File:.........."^filename) in
  let inchan = 
    try open_in filename 
    with e -> error ("Unable to open file: " ^ filename) in
  let rec read_line line_number annot_list =
    try 
      let line = input_line inchan in
      let rec find_annot col_number old_list =
        let match_pos =
          try
          (*NOTE: no asterisk allowed in Bug string*)
            Str.search_forward
            (Str.regexp "/\\*{\\(Bug\\):\\([^*]*\\)\\*/") line col_number
          with Not_found -> -1 in
        if match_pos = -1 then old_list
        else
          find_annot (Str.match_end ())
            (((line_number, (Str.match_end ()) + 1), Str.matched_group 2 line)::old_list) in
      let new_list = find_annot 0 annot_list in
      read_line (line_number+1) new_list
    with End_of_file -> let _ = close_in inchan in annot_list in
  read_line 1 [] *)
  let inx = In_channel.create filename in 
  let lexbuf = Lexing.from_channel inx in
  let rev_mark_list =
    try Annparser.prog Annlexer.read lexbuf with 
      Annparser.Error -> 
        let _ = hdebug "Parsing failed. Annotations ignored in file: " pr_str filename in 
        [] in

  (*print all marks*)
  let _ = List.iter (List.rev (List.map rev_mark_list ~f:Ann.str_of_mark)) ~f:print_endline in
  List.rev rev_mark_list

let func_map ann =
  "__assert_integer_overflow"

let apply_annotation ann_str instr modul=
  match instr with Instr inx ->
    let insert_pos = LL.instr_succ inx in
    let builder = LL.builder_at (LL.module_context modul) insert_pos in
    let assert_func_opt = LL.lookup_function (func_map ann_str) modul in
    let _ = match assert_func_opt with
    | None -> ()
    | Some assert_func ->
        let assert_ins = LL.build_call assert_func 
            (Array.create ~len:1 inx) "" builder in 
        print_endline "!----------!" in

    print_endline (ann_str^"...."^(LL.string_of_llvalue inx))

let instrument_bug_annotation ann_marks (modul: LL.llmodule) : unit =
  (* TODO: fill code here.
     See module llsimp.ml, function elim_instr_intrinsic_lifetime ...
     for how to manipulating LLVM bitcode *)

(*  let _ = print "INSTRUMENT BUG ANNOTATION" in

  let sorted_anns = List.rev annotations in
  let _ =
    List.iter ~f:(fun ((line, col), ann) ->
      print_endline (ann^"----------"^(pr_int line)^"------------"^(pr_int col))
    ) sorted_anns in *)

  (* map each instr to (line, col) * tag * instr *)
  let finstr = Some (fun acc instr ->
    let pos = LS.position_of_instr instr in
    match pos with
    | None -> acc
    | Some p -> 
        match acc with
        | [] -> (make_tagged_ins p.pos_line_start p.pos_col_start 1 instr)::acc
        | hd::tl -> 
            (make_tagged_ins p.pos_line_start p.pos_col_start (hd.tag + 1) instr)::acc) in
  let tagged_ins = deep_fold_module ~finstr [] modul in
  let compare ins1 ins2 =
    let p1 = (ins1.line, ins1.col) in
    let p2 = (ins2.line, ins2.col) in
    if Poly.(p1 > p2) then 1
    else if Poly.(p1 < p2) then -1
    else 0 in

  let sorted_ins = List.stable_sort ~compare tagged_ins in
  let _ = List.iter ~f:(fun tin ->
    match tin.ins with 
    | Instr inx ->  
      print_endline ((LL.string_of_llvalue inx) ^ " +++ " ^ (pr_int tin.line) ^ " .. "
        ^ (pr_int tin.col) ^ " .. tag: " ^ (pr_int tin.tag) )
(*      let pos = LS.position_of_instr instr in
        match pos with
        | None -> ()
        | Some p -> print_endline ((LL.string_of_llvalue inx)^"++++ "^
                         (pr_int line) ^ "__" ^
                         (pr_int col)) *)
  ) sorted_ins in
  
  ()

  (*let rec resolve anns instr_wps =
    match anns with
    | [] -> ()
    | (pa, stra)::ta ->
        match instr_wps with
        | [] -> ()
        | (pi, instr)::ti -> 
            if Poly.(pa > pi) then resolve anns ti
            else
              match instr with Instr inx ->
                match instr_opcode instr with
                | LO.Load | LO.SExt -> resolve anns ti
                | _ -> let _ = apply_annotation stra instr modul in
                  resolve ta ti in
  let _ = resolve ann_marks sorted_instr_wps in
  print_endline ("***********************\n" ^ 
                (LL.string_of_llmodule modul) ^
                "***********************") *)

let instrument_bitcode ann_marks filename (modul: LL.llmodule) : unit =
  instrument_bug_annotation ann_marks modul
