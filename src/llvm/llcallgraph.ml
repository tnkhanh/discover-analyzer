(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Libdiscover
open Printer
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

let pr_scblocks (scb : scblocks) : string = pr_list ~f:block_name scb

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
 ** sparse control flow graph
 *******************************************************************)
