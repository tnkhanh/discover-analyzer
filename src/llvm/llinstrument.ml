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
  let mark_list =
    Annparser.prog Annlexer.read lexbuf in
  mark_list
(*    try Annparser.program Annlexer.read lexbuf with
    | SyntaxError _ -> []
    | Annparser.Error -> []

  let _ = In_channel.close inx in *)
(*  mark_list *)

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
  ()
  (* print_endline (Ann.str_of_prog ann_marks) *)
(*  let _ = print "INSTRUMENT BUG ANNOTATION" in

  let sorted_anns = List.rev annotations in
  let _ =
    List.iter ~f:(fun ((line, col), ann) ->
      print_endline (ann^"----------"^(pr_int line)^"------------"^(pr_int col))
    ) sorted_anns in
  let finstr = Some (fun acc instr ->
    let pos = LS.position_of_instr instr in
    match pos with
    | None -> acc
    | Some p -> ((p.pos_line_start, p.pos_col_start), instr)::acc) in
  let instr_wps = deep_fold_module ~finstr [] modul in
  let compare (p1, inx1) (p2, inx2) =
    if Poly.(p1 > p2) then 1
    else if Poly.(p1 < p2) then -1
    else 0 in

  let sorted_instr_wps = List.stable_sort ~compare instr_wps in
  let _ = List.iter ~f:(fun instr_wp ->
    match instr_wp with ((line, col), instr) ->
    match instr with Instr inx
      ->  let pos = LS.position_of_instr instr in
          match pos with
          | None -> ()
          | Some p 
            -> print_endline ((LL.string_of_llvalue inx)^"++++ "^
                         (pr_int line) ^ "__" ^
                         (pr_int col))
  ) instr_wps in

  let rec resolve anns instr_wps =
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
  let _ = resolve sorted_anns sorted_instr_wps in
  print_endline ("***********************\n" ^ 
                (LL.string_of_llmodule modul) ^
                "***********************") *)

let instrument_bitcode ann_marks filename (modul: LL.llmodule) : unit =
  instrument_bug_annotation ann_marks modul
