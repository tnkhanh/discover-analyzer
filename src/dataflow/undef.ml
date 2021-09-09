(******************************************************************
 ** Author: Ta Quang Trung
 ** Date: 2020
 ******************************************************************)

open Core
open Dcore
open Llir

module DF = Dataflow
module LL = Llvm
module LO = Llvm.Opcode
module LC = Llvm.Icmp
module LA = List.Assoc
module BG = Bug
module SP = Set.Poly

(*******************************************************************
 ** Abstract domain for the analysis
 *******************************************************************)

module UndefDomain = struct

  type undef = {
    undef_pointers : llvalues;
    undef_values : llvalues;
  }

  let mk_undef pointers values =
    { undef_pointers = pointers;
      undef_values = values; }

  let pr_undef (ud: undef) =
    let pointers = ud.undef_pointers |> pr_list_curly pr_value in
    let values = ud.undef_values |> pr_list_square pr_value in
    let output = pointers ^ ", " ^ values in
    beautiful_concat ~sep:" " (String.split ~on:' ' output)

  let equal_undef (ud1: undef) (ud2: undef) : bool =
    List.length ud1.undef_pointers = List.length ud2.undef_pointers &&
    List.is_subset ud1.undef_pointers ud2.undef_pointers ~equal:equal_llvalue &&
    List.length ud1.undef_pointers = List.length ud2.undef_pointers &&
    List.is_subset ud1.undef_values ud2.undef_values ~equal:equal_llvalue

  let lequal_undef (ud1: undef) (ud2: undef) : bool =
    let num_pointers1 = List.length ud1.undef_pointers in
    let num_pointers2 = List.length ud2.undef_pointers in
    if num_pointers1 < num_pointers2 then true
    else if num_pointers1 > num_pointers2 then false
    else List.length ud1.undef_values <= List.length ud2.undef_values

  let is_undef_pointer (ud: undef) (v: llvalue) : bool =
    List.mem ud.undef_pointers v ~equal:equal_llvalue

  let is_undef_value (ud: undef) (v: llvalue) : bool =
    List.mem ud.undef_values v ~equal:equal_llvalue

  let insert_undef_pointer (ud: undef) (v: llvalue) : undef =
    let pointers = List.insert_dedup ud.undef_pointers v ~equal:equal_llvalue in
    {ud with undef_pointers = pointers}

  let insert_undef_value (ud: undef) (v: llvalue) : undef =
    let values = List.insert_dedup ud.undef_values v ~equal:equal_llvalue in
    {ud with undef_values = values}

  let remove_undef_pointer (ud: undef) (v: llvalue) : undef =
    let pointers = List.remove ud.undef_pointers v ~equal:equal_llvalue in
    {ud with undef_pointers = pointers}

  let remove_undef_value (ud: undef) (v: llvalue) : undef =
    let values = List.remove ud.undef_values v ~equal:equal_llvalue in
    {ud with undef_values = values}

end

module UD = UndefDomain

(*******************************************************************
 ** Core data transfer modules
 *******************************************************************)

module UndefData = struct

  type t = UD.undef

end

module UndefTransfer : DF.ForwardDataTransfer= struct

  include UndefData
  include DF.DataUtilGenerator(UndefData)

  let analysis = DfaUndef

  (*******************************************************************
   ** Handling abstract data
   *******************************************************************)

  let least_data = UD.mk_undef [] []

  let pr_data d = UD.pr_undef d

  let pr_data_checksum = pr_data

  let copy_data d =
    d

  let subst_data ?(sstv: substv = []) ?(sstve: substve = [])
        ?(sste: subste = []) (d: t) : t =
    let pointers = List.map ~f:(subst_value sstv) d.undef_pointers in
    let values = List.map ~f:(subst_value sstv) d.undef_values in
    UD.mk_undef pointers values

  let equal_data = UD.equal_undef

  let lequal_data (a: t)  (b: t) : bool =
    UD.lequal_undef a b

  let merge_data ?(widen=false) (a: t) (b: t) : t =
    let equal = equal_llvalue in
    let pointers = List.concat_dedup a.undef_pointers b.undef_pointers ~equal in
    let values = List.concat_dedup a.undef_values b.undef_pointers ~equal in
    UD.mk_undef pointers values

  (* FIXME: fix this later *)
  let join_data (a: t) (b: t) : t =
    merge_data a b

  (*******************************************************************
   ** Core analysis functions
   *******************************************************************)

  let clean_irrelevant_info_from_data prog func (d: t) : t =
    let pointers = List.exclude ~f:is_local_llvalue d.undef_pointers in
    let values = List.exclude ~f:is_local_llvalue d.undef_values in
    UD.mk_undef pointers values

  let clean_info_of_vars (input: t) (vs: llvalues) : t =
    (* TODO: implement later *)
    input

  let is_data_satisfied_predicate (d: t) (p: predicate) : bool =
    true

  let refine_data_by_predicate ?(widen=false) (d: t) (p: predicate) : t =
    d

  let prepare_callee_input penv instr callee args input : t =
    input

  let compute_callee_output_exns penv instr callee args input fsum : t * exns =
    (input, [])

  let prepare_thrown_exception_data penv exn_ptr tinfo input : t =
    input

  let compute_catch_exception_data penv instr ptr input exn : t =
    input

  let need_widening func : bool =
    false

  let analyze_global (global: global) (input: t) : t =
    (* TODO: default behavior, implement later if necessary *)
    input

  let analyze_instr ?(widen=false) (penv: prog_env) fenv instr (input: t) : t =
    match instr_opcode instr with
    | LO.Unreachable -> least_data
    | LO.Alloca ->
      let dst = dst_of_instr_alloca instr in
      UD.insert_undef_pointer input dst
    | LO.Store ->
      let src = src_of_instr_store instr in
      let dst = dst_of_instr_store instr in
      if UD.is_undef_value input src then
        UD.insert_undef_pointer input dst
      else UD.remove_undef_pointer input dst
    | LO.Load ->
      let src = src_of_instr_load instr in
      let dst = dst_of_instr_load instr in
      if UD.is_undef_pointer input src then
        UD.insert_undef_value input dst
      else UD.remove_undef_value input dst
    | LO.GetElementPtr ->
      (* TODO: need to handle GEP *)
      input
    | LO.SExt ->
      let src = src_of_instr_sext instr in
      let dst = dst_of_instr_sext instr in
      if UD.is_undef_value input src then
        UD.insert_undef_value input dst
      else UD.remove_undef_value input dst
    | LO.BitCast ->
      let src = src_of_instr_bitcast instr in
      let dst = dst_of_instr_bitcast instr in
      if UD.is_undef_value input src then
        UD.insert_undef_value input dst
      else UD.remove_undef_value input dst
    | LO.PHI ->
      let srcs = src_of_instr_phi instr in
      if List.exists ~f:(UD.is_undef_value input) srcs then
        let dst = dst_of_instr_phi instr in
        UD.insert_undef_value input dst
      else input
    | _ -> input

  (*******************************************************************
   ** Bug and assertions
   *******************************************************************)

  let check_bug (fenv: func_env) (bug: BG.bug) : ternary =
    (* TODO: implement later if necessary *)
    Unkn

  let count_assertions (prog: program) : int =
    (* TODO: implement later if necessary *)
    0

  let check_assertions (penv: prog_env) func : int =
    (* TODO: implement later if necessary *)
    0

  (*******************************************************************
   ** Pre- and post- analysis
   *******************************************************************)

end

(*******************************************************************
 ** Main analysis module
 *******************************************************************)

module UndefAnalysis = struct
  include UndefTransfer
  include DF.ForwardDataFlow(UndefTransfer)
end
