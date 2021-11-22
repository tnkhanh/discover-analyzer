(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core
open Libdiscover
open Llir
module LL = Llvm
module BG = BlockGraph
module CG = CallGraph
module IG = InstrGraph
module CallGraphSCC = Graph.Components.Make (CG)
module BlockGraphSCC = Graph.Components.Make (BG)
module CGSCC = CallGraphSCC
module BGSCC = BlockGraphSCC

(*******************************************************************
 ** block graph
 *******************************************************************)

type strongly_connected_blocks = block list
type strongly_connected_funcs = func list
type scblocks = strongly_connected_blocks
type scfuncs = strongly_connected_funcs
type scblockss = scblocks list
type scfuncss = scfuncs list

let sprint_scblocks (scb : scblocks) : string = sprint_list ~f:block_name scb

let build_block_graph (prog : program) (blks : blocks) : BG.t =
  let bgraph = BG.create () in
  let _ =
    List.iter
      ~f:(fun blk ->
        let sblks = get_succeeding_blocks prog blk in
        List.iter ~f:(fun sb -> BG.add_edge bgraph blk sb.sblk_block) sblks)
      blks in
  bgraph
;;

let get_block_graph (prog : program) (f : func) : BG.t =
  let compute () : BG.t =
    let blks = blocks_of_func f in
    build_block_graph prog blks in
  Hashtbl.find_or_compute prog.prog_func_data.pfd_block_graph ~key:f ~f:compute
;;

let get_strongly_connected_blocks (prog : program) (blks : blocks) : scblockss =
  let bgraph = build_block_graph prog blks in
  BGSCC.scc_list bgraph
;;

let shortest_block_distance bg (src : block) (dst : block) : int option =
  try
    let _, distance = BG.Dijkstra.shortest_path bg src dst in
    Some distance
  with _ -> None
;;

(*******************************************************************
 ** reachability graph
 *******************************************************************)

let build_reachability_graph (prog : program) : IG.t =
  let rg = prog.prog_instr_graph in
  let _ =
    if IG.is_empty rg
    then (
      let visit_func f =
        let src = llvalue_of_func f in
        match first_instr_of_func f with
        | None -> None
        | Some i ->
          let dst = llvalue_of_instr i in
          let _ = IG.add_edge_e rg (IG.E.create src IG.NextInstr dst) in
          None in
      let visit_instr instr =
        let src = llvalue_of_instr instr in
        let next_instrs =
          match instr_succ instr with
          | Some instr' -> [ instr' ]
          | None ->
            (match instr_opcode instr with
            | LO.Br | LO.IndirectBr ->
              let next_blocks =
                match branch_of_instr_br instr with
                | None -> []
                | Some (`Conditional (_, b1, b2)) -> [ b1; b2 ]
                | Some (`Unconditional b) -> [ b ] in
              List.fold_left
                ~f:(fun acc b ->
                  match first_instr_of_block b with
                  | None -> acc
                  | Some i' -> acc @ [ i' ])
                ~init:[]
                next_blocks
            | _ -> []) in
        let _ =
          List.iter
            ~f:(fun instr' ->
              let dst = llvalue_of_instr instr' in
              IG.add_edge_e rg (IG.E.create src IG.NextInstr dst))
            next_instrs in
        match instr_opcode instr with
        | LO.Call | LO.Invoke ->
          let callee = callee_of_instr_func_call instr in
          let dst = llvalue_of_func callee in
          IG.add_edge_e rg (IG.E.create src IG.Callee dst)
        | _ -> () in
      deep_iter_program
        ~ffunc:(Some visit_func)
        ~finstr:(Some visit_instr)
        prog) in
  rg
;;

(* let is_reachable_instr prog (src: instr) (dst: instr) : bool =
 *   let ig = prog.prog_instr_graph in
 *   let src, dst = llvalue_of_instr src, llvalue_of_instr dst in
 *   let pchecker = IG.Pathcheck.create ig in
 *   IG.Pathcheck.check_path pchecker src dst *)

let shortest_callee_distance rg (caller : func) (callee : func) : int option =
  try
    let src, dst = llvalue_of_func caller, llvalue_of_func callee in
    let _, distance = IG.Dijkstra.shortest_path rg src dst in
    Some distance
  with _ -> None
;;

(*******************************************************************
 ** sparse control flow graph
 *******************************************************************)
