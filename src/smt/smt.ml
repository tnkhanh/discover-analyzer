(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Core

(* open Globals *)
open Libdiscover
module SI = Slir
module LI = Llir
module Z3SL = Z3.Z3SL
module Z3LL = Z3.Z3LL

type smt_solver =
  | SolverZ3
  | SolverOmega

let time_smt = ref 0.
let smt_solver = ref SolverZ3

(*******************************************************************
 ** utilities
 *******************************************************************)

let set_solver name =
  if String.equal name "oc"
  then smt_solver := SolverOmega
  else if String.equal name "z3"
  then smt_solver := SolverZ3
  else ()
;;

let pr_solver solver =
  match solver with
  | SolverOmega -> "Omega"
  | SolverZ3 -> "Z3"
;;

let stop_solver () =
  match !smt_solver with
  | SolverZ3 -> Z3.stop_solver ()
  | _ -> ()
;;

let restart_solver () =
  match !smt_solver with
  | SolverZ3 -> Z3.restart_solver ()
  | _ -> ()
;;

(*******************************************************************
 ** SMT support for SLIR
 *******************************************************************)

module SmtSL = struct
  let check_sat ?(prog = None) ?(mvars = []) (fs : SI.pure_forms)
      : SI.smt_result
    =
    let check_sat =
      match !smt_solver with
      | SolverZ3 -> Z3SL.check_sat ~prog ~mvars
      | _ -> herror "check_sat: unknown solver" pr_solver !smt_solver in
    Sys.record_runtime (fun () -> check_sat fs) time_smt
  ;;

  let is_sat ?(prog = None) (fs : SI.pure_forms) : bool =
    let res, _ = check_sat ~prog fs in
    match res with
    | Some true -> true
    | _ -> false
  ;;

  let is_unsat ?(prog = None) (fs : SI.pure_forms) : bool =
    let res, _ = check_sat ~prog fs in
    match res with
    | Some false -> true
    | _ -> false
  ;;

  let check_imply ?(prog = None) ?(norm = false) f1 f2 : bool option =
    let check_imply =
      match !smt_solver with
      | SolverZ3 -> Z3SL.check_imply
      | _ -> herror "check_imply: unknown solver" pr_solver !smt_solver in
    check_imply f1 f2
  ;;

  (* NOTE: should use horn clause solving, or other solving technique? *)
  let check_sat_horn ?(prog = None) ?(mvars = []) (ents : SI.entailments)
      : SI.smt_result
    =
    (* FIXME: need to transform entailments into pure horn clauses *)
    let fs =
      List.map
        ~f:(fun e ->
          let lhs =
            match SI.is_f_pure e.SI.ent_lhs with
            | true -> SI.extract_pure_form e.SI.ent_lhs
            | false -> herror "check_sat_horn: not a pure body" SI.sprint_ent e
          in
          let rhs =
            match SI.is_f_pure e.SI.ent_rhs with
            | true -> SI.extract_pure_form e.SI.ent_rhs
            | _ -> herror "check_sat_horn: not a pure horn" SI.sprint_ent e
          in
          let vs = SI.merge_vs [ SI.fv_pf lhs; SI.fv_pf rhs ] in
          let f = SI.mk_pdisj [ SI.mk_pneg lhs; rhs ] in
          if List.is_empty vs then f else SI.mk_pforall vs f)
        ents in
    Z3SL.check_sat ~prog ~logic:"HORN" ~mvars fs
  ;;
end

(*******************************************************************
 ** SMT support for LLIR
 *******************************************************************)

module SmtLL = struct
  let check_sat (p : LI.predicate) : bool option =
    let check_sat =
      match !smt_solver with
      | SolverZ3 -> Z3LL.check_sat
      | _ -> herror "check_sat: unknown solver" pr_solver !smt_solver in
    Sys.record_runtime (fun () -> check_sat p) time_smt
  ;;

  let is_sat ?(prog = None) (p : LI.predicate) : bool =
    match check_sat p with
    | Some true -> true
    | _ -> false
  ;;

  let is_unsat ?(prog = None) (p : LI.predicate) : bool =
    match check_sat p with
    | Some false -> true
    | _ -> false
  ;;
end
