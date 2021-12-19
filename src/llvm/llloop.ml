(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Llir
module LL = Llvm
module LG = Llcallgraph
module BG = BlockGraph
module BGSCC = LG.BlockGraphSCC

let get_loop_blocks loop : block list = loop.loop_head :: loop.loop_body

let find_loop_head (prog : program) (scb : LG.scblocks) : block option =
  (* let _ = printh "Find_loop_head in: " LG.pr_scblocks scb in *)
  let head_blks =
    List.fold_left
      ~f:(fun acc blk ->
        let pblks = get_preceding_blocks prog blk in
        let has_outside_preceding =
          List.exists
            ~f:(fun pb -> not (List.mem scb pb.pblk_block ~equal:( == )))
            pblks in
        if has_outside_preceding then acc @ [ blk ] else acc)
      ~init:[] scb in
  match head_blks with
  | [] -> None
  | [ blk ] -> Some blk
  | _ ->
    let msg =
      "Too many loop heads: " ^ block_names head_blks ^ "\n"
      ^ "  Stop creating loop!" in
    let _ = warning msg in
    None
;;

let find_loop_exit (prog : program) (scb : LG.scblocks) : blocks =
  let scblocks = SP.of_list scb in
  let exit_blocks =
    List.fold_left
      ~f:(fun acc1 blk ->
        let sblks = get_succeeding_blocks prog blk in
        List.fold_left
          ~f:(fun acc2 sblk ->
            let blk_succ = sblk.sblk_block in
            if SP.mem scblocks blk_succ then acc2 else SP.add acc2 blk_succ)
          ~init:acc1 sblks)
      ~init:SP.empty scb in
  SP.to_list exit_blocks
;;

let update_loop_info (prog : program) loop : loop =
  let exit_reachables =
    List.fold_left
      ~f:(fun acc blk ->
        let blks = get_reachable_blocks prog blk in
        List.concat_dedup acc blks ~equal:equal_block)
      ~init:loop.loop_exit loop.loop_exit in
  { loop with loop_exit_reachables = exit_reachables }
;;

let find_loop_in_blocks (prog : program) (blks : blocks) : loops =
  let scbs = LG.get_strongly_connected_blocks prog blks in
  (* let _ = debughc "SCC blocks: " (pr_items LG.pr_scblocks) scbs in *)
  let loops =
    List.fold_left
      ~f:(fun acc scb ->
        if List.length scb <= 1
        then acc
        else (
          match find_loop_head prog scb with
          | None -> acc
          | Some head ->
            let body = List.filter ~f:(( != ) head) scb in
            let exit = find_loop_exit prog scb in
            let loop = mk_loop ~head ~body ~exit in
            let loop = update_loop_info prog loop in
            acc @ [ loop ]))
      ~init:[] scbs in
  (* let _ = debughc "All loops: " (pr_items pr_loop) loops in *)
  loops
;;

let find_loop_in_func (prog : program) (func : func) : loops =
  let blks = blocks_of_func func in
  find_loop_in_blocks prog blks
;;

let get_loops_of_func (prog : program) (func : func) : loops =
  Hashtbl.find_or_compute prog.prog_func_data.pfd_loops ~key:func ~f:(fun () ->
      find_loop_in_func prog func)
;;

let find_innermost_loop_of_block (prog : program) blk : loop option =
  let find_loop v =
    let rec find_innermost_loop loop blk : loop =
      let loop_opt =
        List.find
          ~f:(fun lp -> List.mem ~equal:equal_block (get_loop_blocks lp) blk)
          loop.loop_inners in
      match loop_opt with
      | None -> loop
      | Some nloop -> find_innermost_loop nloop blk in
    let func = func_of_block blk in
    let loops = get_loops_of_func prog func in
    let loop_opt =
      List.find
        ~f:(fun lp -> List.mem ~equal:equal_block (get_loop_blocks lp) blk)
        loops in
    match loop_opt with
    | None -> None
    | Some loop -> Some (find_innermost_loop loop blk) in
  Hashtbl.find_or_compute
    prog.prog_loop_data.pld_innermost_loop_containing_block
    ~f:(fun () -> find_loop blk)
    ~key:blk
;;

let find_innermost_loop_of_llvalue (prog : program) (v : value) : loop option =
  let find_loop v =
    if is_llvalue_instr v
    then (
      let blk = block_of_instr (mk_instr v) in
      find_innermost_loop_of_block prog blk)
    else None in
  Hashtbl.find_or_compute
    prog.prog_loop_data.pld_innermost_loop_containing_value
    ~f:(fun () -> find_loop v)
    ~key:v
;;

let is_loop_updated_instr prog instr : bool =
  let check_instr instr =
    let func, blk = func_of_instr instr, block_of_instr instr in
    let loops = get_loops_of_func prog func in
    List.exists
      ~f:(fun lp ->
        List.exists ~f:(equal_block blk) (lp.loop_head :: lp.loop_body))
      loops in
  Hashtbl.find_or_compute prog.prog_loop_data.pld_loop_updated_instr
    ~f:(fun () -> check_instr instr)
    ~key:instr
;;

let is_loop_head_instr prog instr : bool =
  let check_instr instr =
    let func, blk = func_of_instr instr, block_of_instr instr in
    let loops = get_loops_of_func prog func in
    List.exists ~f:(fun lp -> equal_block blk lp.loop_head) loops in
  Hashtbl.find_or_compute prog.prog_loop_data.pld_loop_head_instr
    ~f:(fun () -> check_instr instr)
    ~key:instr
;;

let is_loop_updated_llvalue prog v : bool =
  if is_llvalue_instr v then is_loop_updated_instr prog (mk_instr v) else false
;;

let is_loop_head_llvalue prog (v : value) : bool =
  if is_llvalue_instr v then is_loop_head_instr prog (mk_instr v) else false
;;
