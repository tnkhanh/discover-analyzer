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
open Slir

module Entail = struct
  open Proof

  let pr_ptree_core ptree =
    let rec pr_ptcore goal_id ptree =
      let premises =
        match ptree.ptr_sub_trees with
        | [] -> "no premises"
        | _ ->
          ptree.ptr_sub_trees
          |> List.mapi ~f:(fun index pt ->
                 let goal_id = goal_id ^ "." ^ pr_int (index + 1) in
                 pr_ptcore goal_id pt)
          |> String.concat ~sep:"\n" in
      let goal = pr_goal ptree.ptr_goal in
      let rule =
        match ptree.ptr_rule with
        | Some rule -> pr_rule rule
        | None -> "None" in
      ("Goal: " ^ goal_id ^ ":\n")
      ^ (goal ^ "\n\n")
      ^ ("Rule: " ^ rule ^ "\n\n")
      ^ "==>\n\n"
      ^ premises in
    match ptree.ptr_status with
    | Some _ -> pr_ptcore "1" ptree
    | None -> "Proof tree status: Unknown"
  ;;
end

let export_ptree_core ptree = print (Entail.pr_ptree_core ptree)
