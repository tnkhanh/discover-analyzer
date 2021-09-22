(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Dcore

module LL = Llvm
module SI = Slir
module LI = Llir

module Entails= struct

  let dump_data_defn (d: SI.data_defn) : string =
    SI.pr_data_defn d

  let dump_view_defn (v: SI.view_defn) : string =
    SI.pr_view_defn v

  let dump_func_defn (f: SI.func_defn) : string =
    SI.pr_func_defn f

  let dump_reln_defn (r: SI.reln_defn) : string =
    SI.pr_reln_defn r

  let dump_command (c: SI.command) : string =
    match c with
    | SI.CheckSat f -> "CheckSat: " ^ (SI.pr_formula f) ^ ";"
    | SI.ProveEntails ents ->
      let sents = ents |> List.map ~f:(fun ent -> "  " ^ (SI.pr_ent ent)) |>
                  String.concat ~sep:"\n" in
      "ProveEntails:\n" ^ sents ^ ";"
    | SI.InferFrame ent -> "InferFrame: " ^ (SI.pr_ent ent) ^ ";"

  let dump_program (p: SI.program) : string =
    let datas = p.SI.prog_data_defns |> List.map ~f:dump_data_defn in
    let views = p.SI.prog_view_defns |> List.map ~f:dump_view_defn in
    let funcs = p.SI.prog_func_defns |> List.map ~f:dump_func_defn in
    let relns = p.SI.prog_reln_defns |> List.map ~f:dump_reln_defn in
    let commands = p.SI.prog_commands |> List.map ~f:dump_command in
    (datas @ views @ funcs @ relns @ commands) |>
    List.exclude ~f:String.is_empty |>
    String.concat ~sep:"\n\n"

  let export_program ?(export=false) (prog: SI.program) : unit =
    if export then (
      let fname = (Filename.chop_extension !input_file) ^ ".sl" in
      let file = open_out fname in
      let _ = fprintf file "%s\n" (dump_program prog) in
      close_out file
    )

  let export_entailments ?(export=false) ents : unit =
    let prog = SI.mk_program_empty () in
    let commands = [SI.mk_cmd_prove_entailments ents] in
    let prog = {prog with SI.prog_commands = commands} in
    export_program ~export prog

end
