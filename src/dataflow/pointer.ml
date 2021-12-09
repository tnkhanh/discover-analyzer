(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Graph
open Llir
module AS = Assertion
module DF = Dataflow
module LL = Llvm
module LI = Llir
module LT = Llvm.TypeKind
module LV = Llvm.ValueKind
module LO = Llvm.Opcode
module LP = Llloop
module LG = Llcallgraph
module BG = Bug
module Opt = Option
module SP = Set.Poly
module SMT = Smt.SmtLL

(** Properties of the analysis:
   - Flow sensitive (e.g., consider if-then-else branches)
   - Combining may- and must-alias
*)

(*
  - Pointer graph for alias analysis
  - Each vertex is a pointer
  - There are two types of edges: must-alias and may-alias edge
  - Compare two graphs is easy since graph is always constructed by
    adding more edges:
        G1 <= G2 if |E1| <= |E2|
*)

(*******************************************************************
 ** Abstract domain for the analysis
 *******************************************************************)

module PointerGraph = struct
  type direction =
    | To
    | From

  type trace_item =
    (* trace instruction *)
    | TPhi of (blocks * block * instr) (* (sorted list) skip, incoming, current *)
    | TSimple of instr
  (* | TNotAlias of (expr * expr) *)

  type trace =
    { trace_path : trace_item list;
      (* all into account *)
      trace_skip : trace_item list (* some are skipped *)
    }

  type label =
    (* edge label *)
    | Alias of (precision * trace)
    | Pto of direction
    | GEP of (lltype * expr list * direction)
    | Seq of (label list * precision * trace)
  (* capture only Pto and GEP *)

  type alias_path_type =
    | ApFull
    | ApPrefix of label list
    | ApNone

  exception EAliasPath of (alias_path_type * string)
  exception EPairExpr of expr
  exception EUnfoldableLabel

  (* equality comparison *)

  let equal_direction (d1 : direction) (d2 : direction) : bool = d1 == d2

  let equal_trace_item (ti1 : trace_item) (ti2 : trace_item) : bool =
    match ti1, ti2 with
    | TPhi (skip1, incoming1, i1), TPhi (skip2, incoming2, i2) ->
      List.length skip1 = List.length skip2
      && List.is_subset skip1 skip2 ~equal:equal_block
      && equal_block incoming1 incoming2
      && (* equal_instr i1 i2 *)
      equal_block (block_of_instr i1) (block_of_instr i2)
    | TSimple i1, TSimple i2 -> equal_instr i1 i2
    | _ -> false
  ;;

  let equal_trace (tr1 : trace) (tr2 : trace) : bool =
    List.length tr1.trace_path = List.length tr2.trace_path
    && List.is_subset tr1.trace_path tr2.trace_path ~equal:equal_trace_item
    && List.length tr1.trace_skip = List.length tr2.trace_skip
    && List.is_subset tr1.trace_skip tr2.trace_skip ~equal:equal_trace_item
  ;;

  let rec equal_label (l1 : label) (l2 : label) =
    match l1, l2 with
    | Alias (p1, _), Alias (p2, _) -> equal_precision p1 p2
    | Pto d1, Pto d2 -> equal_direction d1 d2
    | GEP (t1, idxs1, d1), GEP (t2, idxs2, d2) ->
      equal_lltype t1 t2
      && List.equal equal_expr idxs1 idxs2
      && equal_direction d1 d2
    | Seq (lbls1, p1, _), Seq (lbls2, p2, _) ->
      List.equal equal_label lbls1 lbls2 && equal_precision p1 p2
    | _ -> false
  ;;

  (* printing *)

  let pr_v = pr_expr

  let pr_prec (p : precision) : string =
    match p with
    | May -> "May"
    | Must -> "Must"
  ;;

  let pr_direc (d : direction) : string =
    match d with
    | To -> "To"
    | From -> "From"
  ;;

  let pr_trace_item (ti : trace_item) : string =
    match ti with
    | TPhi (skip, incoming, i) ->
      let skip_blks =
        let str = pr_list_plain ~sep:"-" ~f:block_name_full skip in
        String.surround_if_not_empty str ~prefix:"-" ~suffix:" " in
      let incoming_blk =
        let str = block_name_full incoming in
        String.surround_if_not_empty str ~prefix:"+" ~suffix:" " in
      let blk = block_of_instr i in
      "(" ^ skip_blks ^ incoming_blk ^ "-> " ^ block_name_full blk ^ ")"
    | TSimple i ->
      let b = block_of_instr i in
      block_name_full b ^ " # " ^ pr_instr i
  ;;

  (* | TNotAlias (e1, e2) -> "-(" ^ (pr_expr e1) ^ ", " ^ (pr_expr e2) ^ ")" *)

  let pr_trace (t : trace) : string =
    let stpath = pr_list_plain ~f:pr_trace_item t.trace_path in
    let stskip =
      match t.trace_skip with
      | [] -> ""
      | _ -> "-" ^ pr_list_curly ~f:pr_trace_item t.trace_skip in
    "[" ^ stpath ^ String.prefix_if_not_empty stskip ~prefix:", " ^ "]"
  ;;

  let rec pr_label ?(trace = true) (l : label) : string =
    match l with
    | Alias (p, t) ->
      pr_prec p ^ "Alias"
      ^ if !mode_deep_debug && trace then pr_trace t else ""
    | Pto d -> "Pto" ^ pr_direc d
    | GEP (t, idxs, d) ->
      let sidxs = pr_list ~sep:"," ~obrace:"" ~cbrace:"" ~f:pr_expr idxs in
      "GEP" ^ pr_direc d ^ "{" ^ pr_type t ^ "," ^ sidxs ^ "}"
    | Seq (lbls, p, t) ->
      pr_prec p
      ^ pr_list ~f:(pr_label ~trace:false) lbls
      ^ if !mode_deep_debug then pr_trace t else ""
  ;;

  let pr_labels lbls = pr_list ~f:pr_label lbls

  let rec subst_label
      ?(sstv : substv = [])
      ?(sstve : substve = [])
      ?(sste : subste = [])
      lbl
      : label
    =
    match lbl with
    | Alias (p, t) -> Alias (p, t)
    | Pto _ -> lbl
    | GEP (t, idxs, d) ->
      let nidxs = List.map ~f:(subst_expr ~sstv ~sstve ~sste) idxs in
      GEP (t, nidxs, d)
    | Seq (lbls, p, t) ->
      let nlbls = List.map ~f:(subst_label ~sstv ~sstve ~sste) lbls in
      Seq (nlbls, p, t)
  ;;

  let mk_trace ?(trace_skip = []) trace_path = { trace_path; trace_skip }

  module Vertex = struct
    type t = expr

    let compare = Poly.compare
    let equal = equal_expr
    let hash = Hashtbl.hash
  end

  module Edge = struct
    type t = label

    let compare = Poly.compare
    let equal = equal_label
    let hash = Hashtbl.hash
    let default = Alias (Must, mk_trace [])
  end

  include Imperative.Digraph.ConcreteLabeled (Vertex) (Edge)

  type vertex = V.t

  (* type vertices = vertex list *)

  type edge = E.t
  type edges = edge list

  type path =
    { path_edges : edge list;
      path_vertices : vertex list;
      path_labels : label list;
      path_flattened_labels : label list;
      path_folded_label : label option;
      path_trace_all : trace_item list;
      path_trace_may : trace_item list;
      path_trace_skip : trace_item list
    }

  let equal_edge (e1 : edge) (e2 : edge) : bool =
    equal_expr (E.src e1) (E.src e2)
    && equal_expr (E.dst e1) (E.dst e2)
    && equal_label (E.label e1) (E.label e2)
  ;;

  let is_sub_label (lbl : label) ~(sub : label) : bool =
    let rec check_sub_labels lbls1 lbls2 : bool =
      match lbls1, lbls2 with
      | _, [] -> true
      | [], _ -> false
      | lbl1 :: nlbls1, lbl2 :: nlbls2 ->
        if equal_label lbl1 lbl2
        then check_sub_labels nlbls1 nlbls2
        else check_sub_labels nlbls1 lbls2 in
    let lbls1, p1 =
      match lbl with
      | Alias (p, _) -> [ lbl ], p
      | Pto _ | GEP _ -> [ lbl ], Must
      | Seq (lbls, p, _) -> lbls, p in
    let lbls2, p2 =
      match sub with
      | Alias (p, _) -> [ lbl ], p
      | Pto _ | GEP _ -> [ lbl ], Must
      | Seq (lbls, p, _) -> lbls, p in
    if p1 == p2 && List.length lbls1 > List.length lbls2
    then check_sub_labels lbls1 lbls2
    else false
  ;;

  let is_edge_of_sub_vertex (e1 : edge) (e2 : edge) : bool =
    let src1, dst1, lbl1 = E.src e1, E.dst e1, E.label e1 in
    let src2, dst2, lbl2 = E.src e2, E.dst e2, E.label e2 in
    if equal_expr src1 src2
    then
      (is_sub_expr dst1 ~sub:dst2 && is_sub_label lbl1 ~sub:lbl2)
      || (is_sub_expr dst2 ~sub:dst1 && is_sub_label lbl2 ~sub:lbl1)
    else if equal_expr dst1 dst2
    then
      (is_sub_expr src1 ~sub:src2 && is_sub_label lbl1 ~sub:lbl2)
      || (is_sub_expr src2 ~sub:src1 && is_sub_label lbl2 ~sub:lbl1)
    else false
  ;;

  let pr_edge (e : edge) : string =
    let src = pr_v (E.src e) in
    let dst = pr_v (E.dst e) in
    let label = pr_label (E.label e) in
    label ^ "(" ^ src ^ ", " ^ dst ^ ")"
  ;;

  let pr_edges = pr_list ~f:pr_edge

  let pr_path (path : path) : string =
    let strs = List.map ~f:pr_edge path.path_edges in
    beautiful_concat ~sep:", " strs
  ;;

  let pr_paths (paths : path list) : string = pr_items ~f:pr_path paths

  let is_alias_edge (e : edge) : bool =
    match E.label e with
    | Alias _ -> true
    | _ -> false
  ;;

  let is_may_alias_edge (e : edge) : bool =
    match E.label e with
    | Alias (May, _) -> true
    | _ -> false
  ;;

  let is_must_alias_edge (e : edge) : bool =
    match E.label e with
    | Alias (Must, _) -> true
    | _ -> false
  ;;

  let is_deref_edge (e : edge) : bool =
    match E.label e with
    | Pto _ -> true
    | _ -> false
  ;;

  let is_gep_edge (e : edge) : bool =
    match E.label e with
    | GEP _ -> true
    | _ -> false
  ;;

  let rec get_label_length (lbl : label) : int =
    match lbl with
    | Alias _ | Pto _ | GEP _ -> 1
    | Seq (lbls, _, _) ->
      List.fold_left ~f:(fun acc l -> acc + get_label_length l) ~init:0 lbls
  ;;

  let get_edge_label_length (e : edge) : int = get_label_length (E.label e)

  let get_precision (lbl : label) : precision =
    match lbl with
    | Alias (p, _) -> p
    | Pto _ | GEP _ -> Must
    | Seq (_, p, _) -> p
  ;;

  let set_precision (p : precision) (lbl : label) : label =
    match lbl with
    | Alias (_, t) -> Alias (p, t)
    | Pto _ | GEP _ -> lbl
    | Seq (lbls, _, t) -> Seq (lbls, p, t)
  ;;

  let is_label_precision_compatible (lbl : label) (p : precision) : bool =
    match get_precision lbl with
    | Must -> true
    | May -> equal_precision p May
  ;;

  let rec is_reversed_label (lbl1 : label) (lbl2 : label) : bool =
    match lbl1, lbl2 with
    | Alias (p1, t1), Alias (p2, t2) ->
      equal_precision p1 p2 && equal_trace t1 t2
    | Pto d1, Pto d2 -> not (equal_direction d1 d2)
    | GEP (t1, idxs1, d1), GEP (t2, idxs2, d2) ->
      check_equiv_type t1 t2
      && List.length idxs1 = List.length idxs2
      && List.for_all2_exn ~f:equal_expr idxs1 idxs2
      && not (equal_direction d1 d2)
    | Seq (lbls1, p1, _), Seq (lbls2, p2, _) ->
      List.length lbls1 = List.length lbls2
      && List.for_all2_exn ~f:is_reversed_label lbls1 lbls2
      && equal_precision p1 p2
    | _ -> false
  ;;

  let mk_reversed_direction (d : direction) : direction =
    match d with
    | To -> From
    | From -> To
  ;;

  let rec mk_reversed_label (lbl : label) : label =
    match lbl with
    | Alias _ -> lbl
    | Pto d -> Pto (mk_reversed_direction d)
    | GEP (t, idxs, d) -> GEP (t, idxs, mk_reversed_direction d)
    | Seq (lbls, p, t) ->
      let nlbls = List.map ~f:mk_reversed_label (List.rev lbls) in
      Seq (nlbls, p, t)
  ;;

  let is_edge_reverse_vertices (e1 : edge) (e2 : edge) : bool =
    let src1, dst1 = E.src e1, E.dst e1 in
    let src2, dst2 = E.src e2, E.dst e2 in
    equal_expr src1 dst2 && equal_expr src2 dst1
  ;;

  let has_consecutive_gep_of_different_direction (lbl : label) : bool =
    let rec check_labels lbls =
      match lbls with
      | [] -> false
      | GEP (t1, _, d1) :: (GEP (t2, _, d2) as lbl2) :: nlbls ->
        if check_equiv_type t1 t2 && not (equal_direction d1 d2)
        then true
        else check_labels (lbl2 :: nlbls)
      | lbl :: nlbls -> check_labels nlbls in
    match lbl with
    | Seq (lbls, _, _) -> check_labels lbls
    | _ -> false
  ;;

  let has_deref_of_different_direction (lbl : label) : bool =
    let has_deref_from = ref false in
    let has_deref_to = ref false in
    match lbl with
    | Seq (lbls, _, _) ->
      let _ =
        List.iter
          ~f:(fun lbl ->
            match lbl with
            | Pto From -> has_deref_from := true
            | Pto To -> has_deref_to := true
            | _ -> ())
          lbls in
      !has_deref_from && !has_deref_to
    | _ -> false
  ;;

  let has_gep_of_different_direction (lbl : label) : bool =
    let has_gep_from = ref false in
    let has_gep_to = ref false in
    match lbl with
    | Seq (lbls, _, _) ->
      let _ =
        List.iter
          ~f:(fun lbl ->
            match lbl with
            | GEP (_, _, From) -> has_gep_from := true
            | GEP (_, _, To) -> has_gep_to := true
            | _ -> ())
          lbls in
      !has_gep_from && !has_gep_to
    | _ -> false
  ;;

  let has_edges_of_different_direction (lbl : label) : bool =
    let has_from = ref false in
    let has_to = ref false in
    match lbl with
    | Seq (lbls, _, _) ->
      let _ =
        List.iter
          ~f:(fun lbl ->
            match lbl with
            | Pto From | GEP (_, _, From) -> has_from := true
            | Pto To | GEP (_, _, To) -> has_to := true
            | _ -> ())
          lbls in
      !has_from && !has_to
    | _ -> false
  ;;

  let has_edges_of_direction_from_then_to (lbl : label) : bool =
    try
      let has_from = ref false in
      match lbl with
      | Seq (lbls, _, _) ->
        let _ =
          List.iter
            ~f:(fun lbl ->
              match lbl with
              | Pto From | GEP (_, _, From) -> has_from := true
              | Pto To | GEP (_, _, To) -> if !has_from then raise (EBool true)
              | _ -> ())
            lbls in
        false
      | _ -> false
    with EBool res -> res
  ;;

  let has_consecutive_edges_of_direction_to_from (lbl : label) : bool =
    let rec check_labels lbls =
      match lbls with
      | [] -> false
      | GEP (_, _, To) :: GEP (_, _, From) :: _ -> true
      | Pto To :: Pto From :: _ -> true
      | _ :: nlbls -> check_labels nlbls in
    match lbl with
    | Seq (lbls, _, _) -> check_labels lbls
    | _ -> false
  ;;

  let mk_trace_simple (instr : instr) : trace = mk_trace [ TSimple instr ]

  let mk_trace_phi (skip : blocks) (incoming : block) instr : trace =
    let skip = List.sort ~compare:Poly.compare skip in
    mk_trace [ TPhi (skip, incoming, instr) ]
  ;;

  let update_trace_with_skip_items (t : trace) (tis : trace_item list) : trace =
    let tskip = List.concat_dedup t.trace_skip tis ~equal:equal_trace_item in
    { t with trace_skip = tskip }
  ;;

  let has_trace_item_of_func (t : trace) (f : func) : bool =
    List.exists
      ~f:(fun ti ->
        match ti with
        | TPhi (_, _, i) -> equal_func f (func_of_instr i)
        | TSimple i -> equal_func f (func_of_instr i))
      t.trace_path
  ;;

  let get_trace (lbl : label) : trace =
    match lbl with
    | Alias (_, t) -> t
    | Pto _ | GEP _ -> mk_trace []
    | Seq (lbls, _, t) -> t
  ;;

  let rec set_trace (t : trace) (lbl : label) : label =
    match lbl with
    | Alias (p, _) -> Alias (p, t)
    | Pto _ | GEP _ -> lbl
    | Seq (lbls, p, _) -> Seq (List.map ~f:(set_trace t) lbls, p, t)
  ;;

  let src_of_path (path : path) : vertex =
    match path.path_edges with
    | [] -> herror "src_of_path: empty path: " pr_path path
    | e :: _ -> E.src e
  ;;

  let dst_of_path (path : path) : vertex =
    match List.last path.path_edges with
    | None -> herror "dst_of_path: empty path: " pr_path path
    | Some e -> E.dst e
  ;;

  let has_only_deref_edge (g : t) (v : vertex) : bool =
    try
      iter_succ_e
        (fun e -> if not (is_deref_edge e) then raise (EBool false))
        g v;
      iter_pred_e
        (fun e -> if not (is_deref_edge e) then raise (EBool false))
        g v;
      true
    with EBool res -> res
  ;;

  let has_only_gep_edge (g : t) (v : vertex) : bool =
    try
      iter_succ_e
        (fun e -> if not (is_gep_edge e) then raise (EBool false))
        g v;
      iter_pred_e
        (fun e -> if not (is_gep_edge e) then raise (EBool false))
        g v;
      true
    with EBool res -> res
  ;;

  let get_traces_of_deref (path : path) : (vertex * trace list) list =
    List.fold_left
      ~f:(fun acc v ->
        match v with
        | Deref _ ->
          let edges =
            List.filter
              ~f:(fun e -> equal_expr v (E.src e) || equal_expr v (E.dst e))
              path.path_edges in
          let traces = List.map ~f:(fun e -> get_trace (E.label e)) edges in
          acc @ [ v, traces ]
        | _ -> acc)
      ~init:[] path.path_vertices
  ;;

  let get_trace_instrs (tr : trace) : instrs =
    List.fold_left
      ~f:(fun acc ti ->
        match ti with
        | TPhi (_, incoming, instr) ->
          let nacc = List.insert_dedup acc instr ~equal:equal_instr in
          fold_left_instrs
            ~f:(fun acc2 i -> List.insert_dedup acc2 i ~equal:equal_instr)
            ~init:nacc incoming
        | TSimple instr -> List.insert_dedup acc instr ~equal:equal_instr)
      ~init:[] tr.trace_path
  ;;

  let is_write_trace (tr : trace) (v : llvalue) : bool =
    let instrs = get_trace_instrs tr in
    List.exists
      ~f:(fun instr ->
        match instr_opcode instr with
        | LO.Store -> equal_llvalue (dst_of_instr_store instr) v
        | _ -> false)
      instrs
  ;;

  let is_read_write_trace (tr : trace) (v : llvalue) : bool =
    let instrs = get_trace_instrs tr in
    List.exists
      ~f:(fun instr ->
        match instr_opcode instr with
        | LO.Store -> equal_llvalue (dst_of_instr_store instr) v
        | LO.Load -> equal_llvalue (src_of_instr_load instr) v
        | _ -> false)
      instrs
  ;;

  let is_reachable_trace prog (tr1 : trace) (tr2 : trace) : bool =
    let instrs1, instrs2 = get_trace_instrs tr1, get_trace_instrs tr2 in
    List.exists
      ~f:(fun instr1 ->
        List.exists
          ~f:(fun instr2 ->
            (not (equal_instr instr1 instr2))
            && is_reachable_instr prog instr1 instr2)
          instrs2)
      instrs1
  ;;

  let is_valid_path_trace prog (g : t) (path : path) : bool =
    (* check path's main blocks *)
    let main_blks, incoming_blks =
      List.fold_left
        ~f:(fun (accm, acci) ti ->
          match ti with
          | TPhi (_, incoming_blk, i) ->
            let blk = block_of_instr i in
            let naccm = List.insert_dedup accm blk ~equal:equal_block in
            let nacci =
              List.insert_dedup acci incoming_blk ~equal:equal_block in
            naccm, nacci
          | TSimple i ->
            let b = block_of_instr i in
            List.insert_dedup accm b ~equal:equal_block, acci)
        ~init:([], []) path.path_trace_all in
    (* check path reachability *)
    let check_read_write_flow () =
      let deref_traces = get_traces_of_deref path in
      List.for_all
        ~f:(fun (dexp, trs) ->
          (* let _ = hdebug "check read_write of dexp: " pr_expr dexp in *)
          let trace_pairs = Math.gen_combinations 2 trs in
          List.for_all
            ~f:(fun trs ->
              match trs with
              | [ tr1; tr2 ] ->
                if not (equal_trace tr1 tr2)
                then (
                  let dvs = collect_llvalue_of_expr dexp in
                  if is_reachable_trace prog tr1 tr2
                  then
                    (not (List.exists ~f:(is_read_write_trace tr1) dvs))
                    || not (List.exists ~f:(is_write_trace tr2) dvs)
                  else if is_reachable_trace prog tr2 tr1
                  then
                    (not (List.exists ~f:(is_read_write_trace tr2) dvs))
                    || not (List.exists ~f:(is_write_trace tr1) dvs)
                  else true)
                else true
              | _ -> true)
            trace_pairs)
        deref_traces in
    (* check path's flow blocks *)
    let check_branch_flow () =
      let cond1 =
        List.for_all
          ~f:(fun ti1 ->
            List.for_all
              ~f:(fun ti2 ->
                match ti1, ti2 with
                | TPhi (skip1, incoming1, i1), TPhi (skip2, incoming2, i2) ->
                  if equal_block (block_of_instr i1) (block_of_instr i2)
                  then
                    List.equal equal_block skip1 skip2
                    && equal_block incoming1 incoming2
                  else true
                | _ -> true)
              path.path_trace_all)
          path.path_trace_all in
      let cond2 =
        List.for_all
          ~f:(fun blki ->
            List.exists
              ~f:(fun blkm ->
                List.mem ~equal:equal_block
                  (get_reachable_blocks prog blki)
                  blkm
                || List.mem ~equal:equal_block
                     (get_reachable_blocks prog blkm)
                     blki)
              main_blks)
          incoming_blks in
      cond1 && cond2
      (* && cond3 *) in
    (* check path's skip flow blocks *)
    let check_skip_blocks () =
      let cond1 =
        List.for_all
          ~f:(fun ti1 ->
            List.for_all
              ~f:(fun ti2 ->
                match ti2 with
                | TPhi _ -> not (equal_trace_item ti1 ti2)
                | _ -> true)
              path.path_trace_skip)
          path.path_trace_all in
      let cond2 =
        List.for_all
          ~f:(fun ti1 ->
            match ti1 with
            | TPhi (skip, _, _) ->
              List.for_all
                ~f:(fun ti2 ->
                  match ti2 with
                  | TSimple i ->
                    let blk = block_of_instr i in
                    List.for_all
                      ~f:(fun skip_blk ->
                        (not (equal_block blk skip_blk))
                        && not
                             (has_unique_path_between_blocks prog blk skip_blk))
                      skip
                  | _ -> true)
                path.path_trace_may
            | _ -> true)
          path.path_trace_may in
      cond1 && cond2 in
    let check_loop_blocks () =
      (* a valid path trace doesn't contains both body blocks
         and other blocks reachable from exit blocks of the same loop *)
      let loops =
        List.fold_left
          ~f:(fun acc blk ->
            match LP.find_innermost_loop_of_block prog blk with
            | Some loop -> List.insert_dedup acc loop ~equal:equal_loop
            | None -> acc)
          ~init:[] main_blks in
      List.for_all
        ~f:(fun lp ->
          (not (List.is_inter main_blks lp.loop_body ~equal:equal_block))
          || not
               (List.is_inter main_blks lp.loop_exit_reachables
                  ~equal:equal_block))
        loops in
    check_read_write_flow () && check_branch_flow () && check_skip_blocks ()
    && check_loop_blocks ()
  ;;

  let fold_precision (p1 : precision) (p2 : precision) : precision =
    if equal_precision p1 p2 then p1 else May
  ;;

  let fold_trace (t1 : trace) (t2 : trace) : trace =
    let trace_path =
      List.concat_dedup t1.trace_path t2.trace_path ~equal:equal_trace_item
    in
    let trace_skip =
      List.concat_dedup t1.trace_skip t2.trace_skip ~equal:equal_trace_item
    in
    mk_trace ~trace_skip trace_path
  ;;

  (* similar to union *)
  let merge_precision (p1 : precision) (p2 : precision) : precision =
    if equal_precision p1 p2 then p1 else May
  ;;

  let merge_trace (t1 : trace) (t2 : trace) : trace = fold_trace t1 t2

  let fold_precision_label (p : precision) (l : label) =
    match l with
    | Alias (p', t) -> Alias (fold_precision p p', t)
    | Pto _ | GEP _ -> Seq ([ l ], p, get_trace l)
    | Seq (lbls, p', t) -> Seq (lbls, fold_precision p p', t)
  ;;

  let fold_trace_label (t : trace) (l : label) =
    match l with
    | Alias (p, t') -> Alias (p, fold_trace t t')
    | Pto _ | GEP _ -> Seq ([ l ], get_precision l, t)
    | Seq (lbls, p, t') -> Seq (lbls, p, fold_trace t t')
  ;;

  let fold_label (l1 : label) (l2 : label) : label option =
    let nlbl =
      match l1, l2 with
      | Alias (p1, t1), _ -> fold_precision_label p1 (fold_trace_label t1 l2)
      | _, Alias (p2, t2) -> fold_precision_label p2 (fold_trace_label t2 l1)
      | Seq (ls1, p1, t1), Seq (ls2, p2, t2) ->
        Seq (ls1 @ ls2, fold_precision p1 p2, fold_trace t1 t2)
      | _, Seq (ls2, p2, t2) ->
        let p1, t1 = get_precision l1, get_trace l1 in
        Seq (l1 :: ls2, fold_precision p1 p2, fold_trace t1 t2)
      | Seq (ls1, p1, t1), _ ->
        let p2, t2 = get_precision l2, get_trace l2 in
        Seq (ls1 @ [ l2 ], fold_precision p1 p2, fold_trace t1 t2)
      | _, _ ->
        let p1, t1 = get_precision l1, get_trace l1 in
        let p2, t2 = get_precision l2, get_trace l2 in
        Seq ([ l1; l2 ], fold_precision p1 p2, fold_trace t1 t2) in
    match nlbl with
    | Alias _ | Pto _ | GEP _ -> Some nlbl
    | Seq (lbls, p, t) ->
      (try
         let stack = Stack.create () in
         let _ =
           List.iter
             ~f:(fun lbl ->
               match Stack.top stack with
               | Some (Pto From) ->
                 (match lbl with
                 | Pto To ->
                   let _ = Stack.pop stack in
                   ()
                 | _ -> Stack.push stack lbl)
               | Some (Pto To) ->
                 (match lbl with
                 | Pto From -> raise EUnfoldableLabel
                 | _ -> Stack.push stack lbl)
               | Some (GEP (t1, idxs1, From)) ->
                 (match lbl with
                 | GEP (t2, idxs2, To) ->
                   if check_equiv_type t1 t2
                      && List.equal equal_expr idxs1 idxs2
                   then (
                     let _ = Stack.pop stack in
                     ())
                   else raise EUnfoldableLabel
                 | _ -> Stack.push stack lbl)
               | Some (GEP (t1, idxs1, To)) ->
                 (match lbl with
                 | GEP (_, _, From) -> raise EUnfoldableLabel
                 | _ -> Stack.push stack lbl)
               | _ -> Stack.push stack lbl)
             lbls in
         let nlbls = List.rev (Stack.to_list stack) in
         let lbl =
           match nlbls with
           | [] -> Alias (p, t)
           | _ -> Seq (nlbls, p, t) in
         Some lbl
       with EUnfoldableLabel -> None)
  ;;

  let fold_labels (lbls : label list) : label option =
    match lbls with
    | [] -> None
    | lbl :: nlbls ->
      (try
         let nlbl =
           List.fold_left
             ~f:(fun acc l ->
               match fold_label acc l with
               | Some nacc -> nacc
               | None -> raise EUnfoldableLabel)
             ~init:lbl nlbls in
         Some nlbl
       with EUnfoldableLabel -> None)
  ;;

  let check_equiv_label (lbl1 : label) (lbl2 : label) : bool =
    let nlbl1 =
      match lbl1 with
      | Seq (lbls, p, _) ->
        (match fold_labels lbls with
        | Some l -> Some (set_precision p l)
        | None -> None)
      | _ -> Some lbl1 in
    let nlbl2 =
      match lbl2 with
      | Seq (lbls, p, _) ->
        (match fold_labels lbls with
        | Some l -> Some (set_precision p l)
        | None -> None)
      | _ -> Some lbl2 in
    match nlbl1, nlbl2 with
    | Some l1, Some l2 -> equal_label l1 l2
    | _ -> false
  ;;

  let check_prefix_seq_label (lbl1 : label) (lbl2 : label) : bool =
    match lbl1, lbl2 with
    | Seq (lbls1, _, _), Seq (lbls2, _, _) ->
      List.is_prefix lbls2 ~prefix:lbls1 ~equal:equal_label
    | _ -> false
  ;;

  let check_suffix_seq_label (lbl1 : label) (lbl2 : label) : bool =
    match lbl1, lbl2 with
    | Seq (lbls1, _, _), Seq (lbls2, _, _) ->
      List.is_suffix lbls2 ~suffix:lbls1 ~equal:equal_label
    | _ -> false
  ;;

  let check_equiv_edge (e1 : edge) (e2 : edge) : bool =
    equal_expr (E.src e1) (E.src e2)
    && equal_expr (E.dst e1) (E.dst e2)
    && check_equiv_label (E.label e1) (E.label e2)
  ;;

  let check_mem_edge_equiv_label (g : t) (edge : edge) : bool =
    let lbl = E.label edge in
    let edges = find_all_edges g (E.src edge) (E.dst edge) in
    List.exists ~f:(fun e -> check_equiv_label lbl (E.label e)) edges
  ;;

  let check_mem_edge_prefix_seq_label (g : t) (edge : edge) : bool =
    let lbl = E.label edge in
    let edges = find_all_edges g (E.src edge) (E.dst edge) in
    List.exists ~f:(fun e -> check_prefix_seq_label lbl (E.label e)) edges
  ;;

  let rec merge_label (l1 : label) (l2 : label) : label option =
    match l1, l2 with
    | Alias (p1, t1), Alias (p2, t2) ->
      Some (Alias (merge_precision p1 p2, merge_trace t1 t2))
    | Pto d1, Pto d2 -> if equal_direction d1 d2 then Some (Pto d1) else None
    | GEP (t1, idxs1, d1), GEP (t2, idxs2, d2) ->
      if check_equiv_type t1 t2
         && List.equal equal_expr idxs1 idxs2
         && equal_direction d1 d2
      then Some (GEP (t1, idxs1, d1))
      else None
    | Seq (lbls1, p1, t1), Seq (lbls2, p2, t2) ->
      if List.length lbls1 = List.length lbls2
      then (
        let lbls =
          List.fold2_exn
            ~f:(fun acc l1 l2 ->
              match merge_label l1 l2 with
              | None -> acc
              | Some l -> acc @ [ l ])
            ~init:[] lbls1 lbls2 in
        if List.length lbls < List.length lbls1
        then None
        else Some (Seq (lbls, merge_precision p1 p2, merge_trace t1 t2)))
      else None
    | _ -> None
  ;;

  let rec flatten_labels (lbls : label list) acc =
    match lbls with
    | [] -> acc
    | Seq (ls, _, _) :: nlbls ->
      let nacc = flatten_labels ls acc in
      flatten_labels nlbls nacc
    | lbl :: nlbls -> flatten_labels nlbls (acc @ [ lbl ])
  ;;

  let mk_path (edges : edge list) : path =
    let vertices =
      match edges with
      | [] -> []
      | e :: _ -> E.src e :: List.map ~f:E.dst edges in
    let lbls = List.map ~f:E.label edges in
    let folded_lbl = fold_labels lbls in
    let trace_all =
      List.fold_left
        ~f:(fun acc lbl ->
          let t = get_trace lbl in
          List.concat_dedup acc t.trace_path ~equal:equal_trace_item)
        ~init:[] lbls in
    let trace_may =
      List.fold_left
        ~f:(fun acc l ->
          match get_precision l with
          | Must -> acc
          | May ->
            let t = get_trace l in
            List.concat_dedup acc t.trace_path ~equal:equal_trace_item)
        ~init:[] lbls in
    let trace_skip =
      List.fold_left
        ~f:(fun acc lbl ->
          let t = get_trace lbl in
          List.concat_dedup acc t.trace_skip ~equal:equal_trace_item)
        ~init:[] lbls in
    { path_edges = edges;
      path_vertices = vertices;
      path_labels = lbls;
      path_flattened_labels = flatten_labels lbls [];
      path_folded_label = folded_lbl;
      path_trace_all = trace_all;
      path_trace_may = trace_may;
      path_trace_skip = trace_skip
    }
  ;;

  let append_path_edge (path : path) (edge : edge) : path =
    let lbl = E.label edge in
    let folded_label =
      match path.path_folded_label with
      | None -> None
      | Some l -> fold_label l lbl in
    let t = get_trace lbl in
    let trace_all =
      List.concat_dedup path.path_trace_all t.trace_path
        ~equal:equal_trace_item in
    let trace_may =
      match get_precision lbl with
      | Must -> path.path_trace_may
      | May ->
        List.concat_dedup path.path_trace_may t.trace_path
          ~equal:equal_trace_item in
    let trace_skip =
      List.concat_dedup path.path_trace_skip t.trace_skip
        ~equal:equal_trace_item in
    { path_edges = path.path_edges @ [ edge ];
      path_vertices = path.path_vertices @ [ E.dst edge ];
      path_labels = path.path_labels @ [ lbl ];
      path_flattened_labels = flatten_labels (path.path_labels @ [ lbl ]) [];
      path_folded_label = folded_label;
      path_trace_all = trace_all;
      path_trace_may = trace_may;
      path_trace_skip = trace_skip
    }
  ;;
end

module PG = PointerGraph
module PE = PG.E
module PV = PG.V
module PC = Path.Check (PointerGraph)

module Weight = struct
  type edge = PG.E.t
  type t = int

  let weight (e : edge) =
    match PG.E.label e with
    | Alias _ | Pto _ | GEP _ -> 1
    | Seq (lbls, _, _) -> List.length lbls
  ;;

  let compare = Int.compare
  let add = Int.( + )
  let zero = 0
end

module BF = Path.BellmanFord (PointerGraph) (Weight)

module PointerDomain = struct
  open PointerGraph

  (* alias type, pointers are maintained as sorted lists *)

  type pgraph = PG.t (* pointer graph *)

  type vpair = PV.t * PV.t (* vertex pair *)

  let must_alias_cache : (expr, exprs) Hashtbl.t = Hashtbl.create (module ExprH)
  let may_alias_cache : (expr, exprs) Hashtbl.t = Hashtbl.create (module ExprH)

  (* printing *)

  let pr_v = PG.pr_v

  (* print and combine edges of same vertices when possible *)
  let pr_pgraph (g : pgraph) : string =
    let aliases, derefs, geps, sequences = ref [], ref [], ref [], ref [] in
    let _ =
      PG.iter_edges_e
        (fun e ->
          let src, dst = pr_v (PE.src e), pr_v (PE.dst e) in
          match PE.label e with
          | Alias (p, t) ->
            if !mode_deep_debug
            then (
              let edge =
                (PG.pr_prec p ^ "Alias" ^ PG.pr_trace t)
                ^ "(" ^ src ^ ", " ^ dst ^ ")" in
              aliases := !aliases @ [ edge ])
            else if Poly.compare src dst <= 0
            then (
              let edge =
                PG.pr_prec p ^ "Alias" ^ "(" ^ src ^ ", " ^ dst ^ ")" in
              aliases := !aliases @ [ edge ])
          | Pto d ->
            if !mode_deep_debug
            then (
              let edge = "Pto" ^ pr_direc d ^ "(" ^ src ^ ", " ^ dst ^ ")" in
              derefs := !derefs @ [ edge ])
            else if d == PG.From
            then (
              let edge = "Pto" ^ "(" ^ src ^ ", " ^ dst ^ ")" in
              derefs := !derefs @ [ edge ])
          | GEP (t, idxs, d) ->
            let sidxs =
              pr_list ~sep:"," ~obrace:"" ~cbrace:"" ~f:pr_expr idxs in
            if !mode_deep_debug
            then (
              let edge =
                ("GEP" ^ pr_direc d ^ "{" ^ pr_type t ^ "," ^ sidxs ^ "}")
                ^ "(" ^ src ^ ", " ^ dst ^ ")" in
              geps := !geps @ [ edge ])
            else if d == PG.From
            then (
              let edge =
                ("GEP" ^ "{" ^ pr_type t ^ "," ^ sidxs ^ "}")
                ^ "(" ^ src ^ ", " ^ dst ^ ")" in
              geps := !geps @ [ edge ])
          | Seq (lbls, p, t) ->
            let edge =
              PG.pr_prec p
              ^ pr_list ~f:PG.pr_label lbls
              ^ "(" ^ src ^ ", " ^ dst ^ ")" in
            sequences := !sequences @ [ edge ])
        g in
    let all_edges =
      List.sorti ~compare:String.compare !aliases
      @ List.sorti ~compare:String.compare !derefs
      @ List.sorti ~compare:String.compare !geps
      @ List.sorti ~compare:String.compare !sequences in
    if List.is_empty all_edges
    then "{Empty PGraph}"
    else beautiful_concat ~sep:", " ~column:75 all_edges
  ;;

  let pr_graph_size (g : pgraph) : string =
    "{ #vertices: "
    ^ pr_int (PG.nb_vertex g)
    ^ ", " ^ "#edges: "
    ^ pr_int (PG.nb_edges g)
    ^ "}"
  ;;

  (* constructor *)

  let least_graph = PG.create ()

  (* comparison *)

  let is_structural_access_edge (e : edge) : bool =
    let rec check_structural_access u v lbls =
      match lbls, u with
      | [], _ -> equal_expr u v
      | Pto To :: nlbls, _ -> check_structural_access (Deref u) v nlbls
      | Pto From :: nlbls, Deref nu -> check_structural_access nu v nlbls
      | GEP (t, idxs, To) :: nlbls, _ ->
        check_structural_access (ElemPtr (u, t, idxs)) v nlbls
      | GEP (t1, idxs1, From) :: nlbls, ElemPtr (nu, t2, idxs2) ->
        if equal_type t1 t2 && List.equal equal_expr idxs1 idxs2
        then check_structural_access nu v nlbls
        else false
      | _ -> false in
    let src, dst, lbl = PE.src e, PE.dst e, PE.label e in
    let lbls =
      match lbl with
      | Pto _ | GEP _ -> [ lbl ]
      | Seq (lbls, _, _) -> lbls
      | _ -> [] in
    check_structural_access src dst lbls
  ;;

  let lequal_pgraph (g1 : pgraph) (g2 : pgraph) : bool =
    try
      let _ =
        PG.iter_edges_e
          (fun e ->
            if (not (is_structural_access_edge e))
               && not (check_mem_edge_equiv_label g2 e)
            then raise (EBool false))
          g1 in
      true
    with EBool res -> res
  ;;

  let equal_pgraph (g1 : pgraph) (g2 : pgraph) : bool =
    lequal_pgraph g1 g2 && lequal_pgraph g2 g1
  ;;

  (* substitution *)

  let subst_pgraph
      ?(sstv : substv = [])
      ?(sstve : substve = [])
      ?(sste : subste = [])
      (g : pgraph)
      : pgraph
    =
    let ng = PG.create () in
    let _ =
      PG.iter_vertex
        (fun v ->
          let nv = subst_expr ~sstv ~sstve ~sste v in
          PG.add_vertex ng nv)
        g in
    let _ =
      PG.iter_edges_e
        (fun e ->
          let src, dst = PE.src e, PE.dst e in
          let src = subst_expr ~sstv ~sstve ~sste src in
          let dst = subst_expr ~sstv ~sstve ~sste dst in
          let lbl = PG.subst_label ~sstv ~sstve ~sste (PE.label e) in
          let ne = PE.create src lbl dst in
          match lbl with
          | Alias _ when equal_expr src dst -> ()
          | _ -> PG.add_edge_e ng ne)
        g in
    ng
  ;;

  (** managing alias graph *)

  let remove_vertex (g : pgraph) (v : expr) : unit =
    try PG.remove_vertex g v with _ -> ()
  ;;

  let remove_edge (g : pgraph) (src : expr) (dst : expr) : unit =
    try PG.remove_edge g src dst with _ -> ()
  ;;

  let insert_alias
      ?(trace = None)
      (g : pgraph)
      (p : precision)
      (v1 : expr)
      (v2 : expr)
      : unit
    =
    if (not (is_expr_null v1))
       && (not (is_expr_null v2))
       && (not (is_expr_undef v1))
       && (not (is_expr_undef v2))
       && not (equal_expr v1 v2)
    then (
      let edges = PG.find_all_edges g v1 v2 in
      if not (List.exists ~f:is_alias_edge edges)
      then (
        let trace =
          match trace with
          | None -> mk_trace []
          | Some t -> t in
        let lbl = PG.Alias (p, trace) in
        PG.add_edge_e g (PE.create v1 lbl v2);
        PG.add_edge_e g (PE.create v2 lbl v1)))
  ;;

  let insert_deref ~(root : expr) ~(deref : expr) (g : pgraph) : unit =
    if (not (is_expr_null root))
       && (not (is_expr_null deref))
       && (not (is_expr_undef root))
       && not (is_expr_undef deref)
    then (
      let edges = PG.find_all_edges g root deref in
      if not (List.exists ~f:is_deref_edge edges)
      then (
        let lbl1 = PG.Pto From in
        let lbl2 = PG.Pto To in
        PG.add_edge_e g (PE.create deref lbl1 root);
        PG.add_edge_e g (PE.create root lbl2 deref)))
  ;;

  let insert_element_ptr
      ~(root : expr)
      ~(rtyp : lltype)
      ~(elemptr : expr)
      ~(idxs : exprs)
      (g : pgraph)
      : unit
    =
    if (not (is_expr_null root))
       && (not (is_expr_null elemptr))
       && (not (is_expr_undef root))
       && not (is_expr_undef elemptr)
    then (
      let edges = PG.find_all_edges g root elemptr in
      let has_same_edge =
        List.exists
          ~f:(fun e ->
            match PE.label e with
            | PG.GEP (t, idxs2, _) ->
              check_equiv_type rtyp t && List.equal equal_expr idxs idxs2
            | _ -> false)
          edges in
      if not has_same_edge
      then (
        let lbl1 = PG.GEP (rtyp, idxs, From) in
        let lbl2 = PG.GEP (rtyp, idxs, To) in
        PG.add_edge_e g (PE.create elemptr lbl1 root);
        PG.add_edge_e g (PE.create root lbl2 elemptr)))
  ;;

  let rec get_bitcast_alias (e : expr) : expr =
    let res =
      match e with
      | Undef _ | Int64 _ | Float _ | String _ | FuncRes _ -> e
      | Var v -> Var (get_root_src_of_bitcast v)
      | OldE e -> OldE (get_bitcast_alias e)
      | Exn e -> Exn (get_bitcast_alias e)
      | Malloc e -> Malloc (get_bitcast_alias e)
      | Deref e -> Deref (get_bitcast_alias e)
      | ElemPtr (e, rtyp, es) ->
        ElemPtr (get_bitcast_alias e, rtyp, List.map ~f:get_bitcast_alias es)
    in
    (* let _ = debug ("- Get top alias: " ^ (pr_expr e) ^ "  -->  " ^ (pr_expr res)) in *)
    res
  ;;

  let get_bitcast_alias_of_llvalue (v : llvalue) : expr =
    get_bitcast_alias (expr_of_llvalue v)
  ;;

  let get_direct_must_aliases (g : pgraph) (v : expr) : exprs =
    let rec get_aliases (queue : exprs) acc =
      match queue with
      | [] -> acc
      | u :: nqueue ->
        let nvs =
          PG.fold_succ_e
            (fun e acc1 ->
              match PE.label e with
              | Alias (Must, _) ->
                let w = PE.dst e in
                if List.not_mem queue w ~equal:equal_expr
                   && List.not_mem acc w ~equal:equal_expr
                then acc1 @ [ w ]
                else acc1
              | _ -> acc1)
            g u [] in
        get_aliases (nqueue @ nvs) (u :: acc) in
    get_aliases [ v ] []
  ;;

  let create_structural_access_edge (e : expr) ~(sub : expr) : label option =
    let rec create_label_sequence e sub acc : label list =
      if equal_expr e sub
      then acc
      else (
        match e with
        | Deref ne ->
          let lbl = Pto From in
          create_label_sequence ne sub (acc @ [ lbl ])
        | ElemPtr (ne, t, idxs) ->
          let lbl = GEP (t, idxs, From) in
          create_label_sequence ne sub (acc @ [ lbl ])
        | _ -> []) in
    let lbls = create_label_sequence e sub [] in
    match lbls with
    | [] -> None
    | _ -> Some (Seq (lbls, Must, mk_trace []))
  ;;

  let connect_sub_vertices (g : pgraph) : pgraph =
    let pathchecker = PC.create g in
    let vertices =
      PG.fold_vertex
        (fun v acc ->
          if PG.succ g v != [] && PG.pred g v != [] then acc @ [ v ] else acc)
        g [] in
    let vpairs = Math.gen_combinations 2 vertices in
    let ng = PG.copy g in
    let _ =
      List.iter
        ~f:(fun vs ->
          match vs with
          | [ v1; v2 ] ->
            let cur_edges = PG.find_all_edges g v1 v2 in
            if List.is_empty cur_edges
            then (
              let exp_subexp =
                if is_sub_expr v1 ~sub:v2
                then Some (v1, v2)
                else if is_sub_expr v2 ~sub:v1
                then Some (v2, v1)
                else None in
              match exp_subexp with
              | None -> ()
              | Some (e, sub) ->
                if not (PC.check_path pathchecker e sub)
                then (
                  match create_structural_access_edge e ~sub with
                  | None -> ()
                  | Some lbl1 ->
                    let lbl2 = mk_reversed_label lbl1 in
                    let edge1 = PE.create e lbl1 sub in
                    let edge2 = PE.create sub lbl2 e in
                    let _ =
                      debug
                        ("Adding edges connecting vertices:\n" ^ "  - "
                       ^ pr_edge edge1 ^ "\n" ^ "  - " ^ pr_edge edge2 ^ "\n")
                    in
                    let _ = PG.add_edge_e ng edge1 in
                    PG.add_edge_e ng edge2)
                else ())
            else ()
          | _ -> ())
        vpairs in
    ng
  ;;

  let simplify_labels_for_alias_check (labels : PE.label list) : PE.label list =
    let rec eliminate_alias (lbls : PE.label list) acc : PE.label list =
      match lbls with
      | [] -> acc
      | Alias _ :: nlbls -> eliminate_alias nlbls acc
      | lbl :: nlbls -> eliminate_alias nlbls (acc @ [ lbl ]) in
    let rec accumulate_gep_indices lbls acc : PE.label list =
      match lbls with
      | [] -> List.rev acc
      | (PG.GEP (t1, idxs1, PG.From) as lbl1) :: nlbls ->
        (match acc with
        | PG.GEP (t2, Int64 i2 :: nidxs2, From) :: nacc ->
          if Int64.( = ) i2 Int64.zero
          then (
            let combined_idxs = idxs1 @ nidxs2 in
            let combined_lbl = PG.GEP (t1, combined_idxs, PG.From) in
            accumulate_gep_indices nlbls (combined_lbl :: nacc))
          else (
            match List.rev idxs1 with
            | Int64 i1 :: nridxs1 ->
              let nidx = Int64 (Int64.( + ) i1 i2) in
              let combined_idxs = List.rev nridxs1 @ [ nidx ] @ nidxs2 in
              let combined_lbl = PG.GEP (t1, combined_idxs, PG.From) in
              accumulate_gep_indices nlbls (combined_lbl :: nacc)
            | _ -> accumulate_gep_indices nlbls (lbl1 :: acc))
        | _ -> accumulate_gep_indices nlbls (lbl1 :: acc))
      | (PG.GEP (t1, Int64 i1 :: nidxs1, PG.To) as lbl1) :: nlbls ->
        (match acc with
        | PG.GEP (t2, idxs2, To) :: nacc ->
          if Int64.( = ) i1 Int64.zero
          then (
            let combined_idxs = idxs2 @ nidxs1 in
            let combined_lbl = PG.GEP (t2, combined_idxs, PG.To) in
            accumulate_gep_indices nlbls (combined_lbl :: nacc))
          else (
            match List.rev idxs2 with
            | Int64 i2 :: nridxs2 ->
              let nidx = Int64 (Int64.( + ) i1 i2) in
              let combined_idxs = List.rev nridxs2 @ [ nidx ] @ nidxs1 in
              let combined_lbl = PG.GEP (t2, combined_idxs, PG.To) in
              accumulate_gep_indices nlbls (combined_lbl :: nacc)
            | _ -> accumulate_gep_indices nlbls (lbl1 :: acc))
        | _ -> accumulate_gep_indices nlbls (lbl1 :: acc))
      | lbl1 :: nlbls -> accumulate_gep_indices nlbls (lbl1 :: acc) in
    let collapse_gep_indices (labels : PE.label list) : PE.label list =
      List.fold_left
        ~f:(fun acc lbl1 ->
          match lbl1 with
          | PG.GEP (t1, idxs1, PG.To) ->
            let res =
              match List.rev acc with
              | PG.GEP (t2, idxs2, PG.From) :: nracc ->
                let len2 = List.length idxs2 in
                if List.length idxs1 > len2
                then (
                  let hd_lst, tl_lst = List.split_n idxs1 (len2 - 1) in
                  if List.equal equal_expr tl_lst idxs2
                  then (
                    let nlbl = PG.GEP (t1, hd_lst, PG.From) in
                    List.rev (nlbl :: nracc))
                  else acc @ [ lbl1 ])
                else acc @ [ lbl1 ]
              | _ -> acc @ [ lbl1 ] in
            res
          | _ -> acc @ [ lbl1 ])
        ~init:[] labels in
    let remove_gep_trailing_indices ~(f : vertex -> bool) labels
        : PE.label list
      =
      let rec remove_indices idxs =
        match idxs with
        | [] -> []
        | idx :: nidxs -> if f idx then remove_indices nidxs else idxs in
      List.fold_left
        ~f:(fun acc lbl ->
          match lbl with
          | PG.GEP (t, idxs, d) ->
            let nidxs = List.rev (remove_indices (List.rev idxs)) in
            if List.is_empty nidxs then acc else acc @ [ PG.GEP (t, nidxs, d) ]
          | _ -> acc @ [ lbl ])
        ~init:[] labels in
    let labels = eliminate_alias labels [] in
    (* let _ = hdebug ~indent:8 "After simplifying labels: " PG.pr_labels labels in *)
    let labels = accumulate_gep_indices labels [] in
    (* let _ = hdebug ~indent:8 "After accumulating gep indices: " PG.pr_labels labels in *)
    let labels = collapse_gep_indices labels in
    (* let _ = hdebug "After collapsing gep indices: " PG.pr_labels labels in *)
    let labels = remove_gep_trailing_indices ~f:is_zero_expr labels in
    (*
     * FIXME: the symbolic_expr affect CPP benchmark
     * Check test case: virtual-diamond-inheritance-1.cpp
     *)
    let labels = remove_gep_trailing_indices ~f:is_symbolic_expr labels in
    labels
  ;;

  (** main algorithm to check for must-alias or may-alias information:
      checking the balance of Pto or ElementPtr edges *)

  let check_path_balance_direction path (p : precision) : alias_path_type =
    (* use a visited list to check balancing *)
    let rec check_balance (labels : PE.label list) (visited : PE.label list) =
      match labels with
      | [] ->
        if List.is_empty visited then ApFull else ApPrefix (List.rev visited)
      | PG.Alias _ :: nlabels -> check_balance nlabels visited
      | (PG.Pto PG.From as l1) :: nlabels ->
        (match visited with
        | PG.GEP (_, _, PG.To) :: _ -> ApNone
        | _ -> check_balance nlabels (l1 :: visited))
      | PG.Pto PG.To :: nlabels ->
        (match visited with
        | [] -> ApNone
        | l2 :: nvisited ->
          (match l2 with
          | PG.Pto PG.From -> check_balance nlabels nvisited
          | _ -> ApNone))
      | (PG.GEP (_, idxs1, PG.From) as l1) :: nlabels ->
        (match visited with
        | PG.GEP (_, _, PG.To) :: _ -> ApNone
        | _ -> check_balance nlabels (l1 :: visited))
      | PG.GEP (t1, idxs1, PG.To) :: nlabels ->
        (match visited with
        | [] -> ApNone
        | l2 :: nvisited ->
          (match l2 with
          | PG.GEP (t2, idxs2, PG.From) when check_equiv_type t1 t2 ->
            if List.length idxs1 = List.length idxs2
            then
              if List.for_all2_exn ~f:equal_expr idxs1 idxs2
              then check_balance nlabels nvisited
              else (
                (* handling pointer arithmetic *)
                let ridxs1, ridxs2 = List.rev idxs1, List.rev idxs2 in
                match ridxs1, ridxs2 with
                | Int64 i1 :: nridxs1, Int64 i2 :: nridxs2 ->
                  if List.for_all2_exn ~f:equal_expr nridxs1 nridxs2
                  then (
                    let idx = mk_expr_int64 (Int64.( - ) i1 i2) in
                    let lbl = PG.GEP (t1, [ idx ], PG.To) in
                    (* let _ = hdebug "New Label: " PG.pr_label lbl in *)
                    check_balance (lbl :: nlabels) nvisited)
                  else ApNone
                | _ -> ApNone)
            else if List.is_prefix idxs2 ~prefix:idxs1 ~equal:equal_expr
                    && List.is_empty nlabels
            then ApPrefix (List.rev visited)
            else ApNone
          | _ -> ApNone))
      | PG.Seq (lbls, _, _) :: nlabels ->
        check_balance (lbls @ nlabels) visited in
    (* let _ = hdebug "Path: " pr_path path in *)
    let labels = path.path_flattened_labels in
    (* let _ = hdebug "Original labels: " PG.pr_labels labels in *)
    let labels = simplify_labels_for_alias_check labels in
    (* let _ = hdebug "After simplifying labels: " PG.pr_labels labels in *)
    check_balance labels []
  ;;

  let check_equiv_edge_of_alias_relation (e1 : edge) (e2 : edge) : bool =
    let lbls1 =
      match PE.label e1 with
      | Seq (lbls, _, _) -> simplify_labels_for_alias_check lbls
      | l -> [ l ] in
    let lbls2 =
      match PE.label e2 with
      | Seq (lbls, _, _) -> simplify_labels_for_alias_check lbls
      | l -> [ l ] in
    List.equal check_equiv_label lbls1 lbls2
  ;;

  let check_mem_edge_of_equiv_alias_relation (g : t) (edge : edge) : bool =
    let edges = find_all_edges g (E.src edge) (E.dst edge) in
    List.exists ~f:(check_equiv_edge_of_alias_relation edge) edges
  ;;

  let check_edge_label_precision edge (p : precision) : bool =
    let lbl = PE.label edge in
    PG.is_label_precision_compatible lbl p
  ;;

  let check_path_deref_direction_to_before_from path =
    let rec check_labels lbls =
      match lbls with
      | [] -> false
      | Pto To :: nlbls ->
        List.exists
          ~f:(fun lbl ->
            match lbl with
            | Pto From -> true
            | _ -> false)
          nlbls
      | lbl :: nlbls -> check_labels nlbls in
    check_labels path.path_flattened_labels
  ;;

  let check_alias_path_type prog g path (p : precision) : alias_path_type =
    let src, dst = PG.src_of_path path, PG.dst_of_path path in
    let _ =
      debug
        ("  Check alias path: " ^ pr_expr src ^ " --> " ^ pr_expr dst ^ "\n"
        ^ String.hindent 4 PG.pr_path path) in
    match check_path_balance_direction path p with
    | ApNone ->
      let _ = debug ~indent:4 "  -> Path contains unbalanced subpath!" in
      ApNone
    | res ->
      if !dfa_path_sensitive && not (PG.is_valid_path_trace prog g path)
      then (
        let _ = debug ~indent:4 "  -> Path contains invalid trace!" in
        ApNone)
      else res
  ;;

  let is_alias_path (apath : alias_path_type) : bool =
    match apath with
    | ApFull -> true
    | ApPrefix lbls ->
      List.for_all
        ~f:(fun lbl ->
          match lbl with
          | GEP (_, idxs, _) -> List.for_all ~f:is_zero_expr idxs
          | _ -> false)
        lbls
    | ApNone -> false
  ;;

  let has_ending_alias_cycle (path : path) (edge : E.t) : bool =
    let rec check_alias_cycle edges v =
      match edges with
      | [] -> false
      | e :: nedges ->
        (match E.label e with
        | Alias _ ->
          let u = E.src e in
          if equal_expr v u then true else check_alias_cycle nedges v
        | _ -> false) in
    check_alias_cycle (List.rev path.path_edges) (E.dst edge)
  ;;

  let has_duplicated_path_access path edge : bool =
    if is_structural_access_edge edge
    then (
      match List.last path.path_edges with
      | Some last_edge when is_structural_access_edge last_edge ->
        let u = PE.src last_edge in
        let v, w = PE.src edge, PE.dst edge in
        if equal_expr u w || is_sub_expr u ~sub:w || is_sub_expr w ~sub:u
        then
          (is_sub_expr v ~sub:u && is_sub_expr v ~sub:w)
          || (is_sub_expr u ~sub:v && is_sub_expr w ~sub:v)
        else false
      | _ -> false)
    else false
  ;;

  let compare_path
      (tbldistance : int BF.H.t)
      (p1 : path)
      (p2 : path)
      (dst : expr)
      : int
    =
    match List.last p1.path_vertices, List.last p2.path_vertices with
    | None, None -> 0
    | None, _ -> -1
    | _, None -> 1
    | Some v1, Some v2 ->
      if equal_expr v1 dst && not (equal_expr v2 dst)
      then -1
      else if equal_expr v2 dst && not (equal_expr v1 dst)
      then 1
      else (
        let d1 = try BF.H.find tbldistance v1 with _ -> 0 in
        let d2 = try BF.H.find tbldistance v2 with _ -> 0 in
        if d1 < d2 then -1 else if d1 > d2 then 1 else 0)
  ;;

  let check_alias_all_paths_bfs
      prog
      (g : pgraph)
      (v1 : expr)
      (v2 : expr)
      (p : precision)
      : bool * path option
    =
    (* update internal edges for vertices of sub-expressions *)
    (* let g = connect_sub_vertices g in *)
    (* check if there is an alias path, using BFS *)
    let sv1, sv2 = pr_expr v1, pr_expr v2 in
    let _ =
      debug ~indent:4 ("Checking alias between: " ^ sv1 ^ " vs. " ^ sv2) in
    let shortest_distances = BF.all_shortest_paths g v2 in
    let rec search_bfs dst (queue : path list) : bool * path option =
      (* let _ = hdebug ~always:true "Path queue BEFORE sorting: " pr_paths queue in *)
      let queue =
        List.sorti
          ~compare:(fun p1 p2 -> compare_path shortest_distances p1 p2 v2)
          queue in
      (* let _ = hdebug ~always:true "AFTER sorting: " pr_paths queue in *)
      match queue with
      | [] -> false, None
      | path :: nqueue ->
        let u = E.dst (List.last_exn path.path_edges) in
        let apath = check_alias_path_type prog g path p in
        if PV.equal dst u
        then
          if is_alias_path apath
          then (
            let _ =
              hdebug ~indent:4
                ("Found " ^ PG.pr_prec p ^ "Alias Path: ")
                PG.pr_path path in
            true, Some path)
          else search_bfs dst nqueue
        else if apath == ApNone
        then search_bfs dst nqueue
        else if is_expr_undef u || is_expr_malloc u || is_expr_malloc u
                || is_expr_function u
        then
          search_bfs dst nqueue
          (* FIXME: rank alias edges as the most important *)
        else (
          let npaths =
            PG.fold_succ_e
              (fun edge acc ->
                if List.mem path.path_edges edge ~equal:equal_edge
                   || List.mem path.path_edges edge
                        ~equal:is_edge_of_sub_vertex
                   || List.mem path.path_edges edge
                        ~equal:is_edge_reverse_vertices
                   || has_ending_alias_cycle path edge
                   || has_duplicated_path_access path edge
                then acc
                else if check_edge_label_precision edge p
                then acc @ [ append_path_edge path edge ]
                else acc)
              g u [] in
          search_bfs dst (nqueue @ npaths)) in
    let src = get_bitcast_alias v1 in
    let dst = get_bitcast_alias v2 in
    let _ =
      if not (equal_expr v1 src)
      then debug ~indent:6 ("BitCast alias: " ^ sv1 ^ " --> " ^ pr_expr src)
    in
    let _ =
      if not (equal_expr v2 dst)
      then debug ~indent:6 ("BitCast alias: " ^ sv2 ^ " --> " ^ pr_expr dst)
    in
    if equal_expr src dst
    then true, None
    else (
      let init_paths =
        try
          PG.fold_succ_e
            (fun e acc ->
              if check_edge_label_precision e p
              then acc @ [ PG.mk_path [ e ] ]
              else acc)
            g src []
        with _ -> [] in
      search_bfs dst init_paths)
  ;;

  let check_alias
      (prog : program)
      (g : pgraph)
      (v1 : expr)
      (v2 : expr)
      (p : precision)
      : bool
    =
    fst (check_alias_all_paths_bfs prog g v1 v2 p)
  ;;

  let check_alias_get_path
      (prog : program)
      (g : pgraph)
      (v1 : expr)
      (v2 : expr)
      (p : precision)
      : bool * path option
    =
    check_alias_all_paths_bfs prog g v1 v2 p
  ;;

  let find_all_aliases_bfs
      ?(filter = fun (u : expr) -> true)
      prog
      g
      (v : expr)
      (p : precision)
      : (expr * path) list
    =
    (* populate pointer graph with extra edges of internal vertices *)
    (* let g = connect_sub_vertices g in *)
    (* check if there is an alias path, using BFS *)
    (* let _ = print ("Find all aliases of: " ^ (pr_expr v)) in *)
    let rec search_bfs (queue : path list) res : (expr * path) list =
      match queue with
      | [] -> res
      | path :: nqueue ->
        let u = E.dst (List.last_exn path.path_edges) in
        let apath = check_alias_path_type prog g path p in
        if filter u && is_alias_path apath
        then (
          let _ =
            hdebug ~indent:4
              ("Found an " ^ PG.pr_prec p ^ "Alias Path: ")
              PG.pr_path path in
          search_bfs nqueue (res @ [ u, path ]))
        else if apath == ApNone
        then search_bfs nqueue res
        else if (* apath == ApPrefix*)
                is_expr_undef u || is_expr_malloc u || is_expr_malloc u
                || is_expr_function u
        then search_bfs nqueue res
        else (
          let npaths =
            PG.fold_succ_e
              (fun edge acc ->
                let w = E.dst edge in
                if List.exists ~f:(fun (x, _) -> equal_expr w x) res
                   || List.mem path.path_edges edge ~equal:equal_edge
                   || List.mem path.path_edges edge
                        ~equal:is_edge_reverse_vertices
                   || has_ending_alias_cycle path edge
                   || has_duplicated_path_access path edge
                then acc
                else if check_edge_label_precision edge p
                then acc @ [ append_path_edge path edge ]
                else acc)
              g u [] in
          search_bfs (nqueue @ npaths) res) in
    let src = get_bitcast_alias v in
    let _ =
      if not (equal_expr v src)
      then
        debug ~indent:6 ("BitCast alias: " ^ pr_expr v ^ " --> " ^ pr_expr src)
    in
    let init_paths =
      try
        PG.fold_succ_e
          (fun e acc ->
            if check_edge_label_precision e p
            then acc @ [ PG.mk_path [ e ] ]
            else acc)
          g src []
      with _ -> [] in
    let res = search_bfs init_paths [] in
    (* let _ = print "   ...done" in *)
    res
  ;;

  let find_all_aliases
      ?(filter = fun (u : expr) -> true)
      prog
      (g : pgraph)
      (v : expr)
      (p : precision)
      : expr list
    =
    let aliases_paths = find_all_aliases_bfs ~filter prog g v p in
    fst (List.unzip aliases_paths)
  ;;

  let find_all_aliases_get_precisions
      ?(filter = fun (u : expr) -> true)
      prog
      (g : pgraph)
      (v : expr)
      (p : precision)
      : (expr * path * precision) list
    =
    let aliases_paths = find_all_aliases_bfs ~filter prog g v p in
    List.map
      ~f:(fun (e, path) ->
        let precision =
          List.fold
            ~f:(fun acc e ->
              PG.fold_precision acc (PG.get_precision (PE.label e)))
            ~init:Must path.path_edges in
        e, path, precision)
      aliases_paths
  ;;

  let merge_alias_vertices (g : pgraph) : pgraph * (expr, expr) Hashtbl.t =
    let tbl_merged_vertices = Hashtbl.create (module ExprH) in
    g, tbl_merged_vertices
  ;;

  let find_connected_vertices
      ?(filter = fun (u : expr) -> true)
      (g : pgraph)
      (v : expr)
      : expr list
    =
    let equal = equal_expr in
    let rec get_connected_vertex queue visited : exprs =
      match queue with
      | [] -> List.remove visited v ~equal
      | u :: nqueue ->
        let nqueue =
          try
            PG.fold_succ
              (fun w acc ->
                if List.not_mem queue w ~equal && List.not_mem visited w ~equal
                then List.insert_dedup acc w ~equal
                else acc)
              g u nqueue
          with e -> nqueue in
        let nvisited = List.insert_dedup visited u ~equal in
        get_connected_vertex nqueue nvisited in
    let cvs = get_connected_vertex [ v ] [] in
    List.filter ~f:filter cvs
  ;;

  (** find alias information using cache *)

  let find_aliases_cache
      (prog : program)
      (g : pgraph)
      (v : expr)
      (p : precision)
      : exprs
    =
    try
      match p with
      | May -> Hashtbl.find_exn may_alias_cache v
      | Must -> Hashtbl.find_exn must_alias_cache v
    with Not_found_s _ ->
      let res = find_all_aliases prog g v p in
      let _ =
        match p with
        | May -> Hashtbl.set may_alias_cache ~key:v ~data:res
        | Must -> Hashtbl.set must_alias_cache ~key:v ~data:res in
      res
  ;;

  let find_deref_paths (g : pgraph) (src : expr) : path list =
    (* let _ = hprint "Find deref paths of " pr_expr src in *)
    let rec is_deref_label lbl =
      match lbl with
      (* TODO: check GEP edges if needed *)
      | Pto To -> true
      | Seq (lbls, _, _) ->
        List.not_empty lbls && List.for_all ~f:is_deref_label lbls
      | _ -> false in
    let rec is_non_deref_label lbl =
      match lbl with
      (* TODO: check GEP edges if needed *)
      | GEP _ -> true
      | Pto From -> true
      | Seq (lbls, _, _) -> List.exists ~f:is_non_deref_label lbls
      | _ -> false in
    let rec search_bfs (queue : path list) acc =
      match queue with
      | [] -> acc
      | path :: nqueue ->
        (match path.path_folded_label with
        | None -> search_bfs nqueue acc
        | Some lbl ->
          let u = E.dst (List.last_exn path.path_edges) in
          if is_deref_label lbl
          then search_bfs nqueue (acc @ [ path ])
          else if List.exists
                    ~f:(fun e -> equal_expr (PE.src e) u)
                    path.path_edges
          then search_bfs nqueue acc
          else (
            let npaths =
              PG.fold_succ_e
                (fun e acc ->
                  if is_non_deref_label (E.label e)
                     || has_ending_alias_cycle path e
                  then acc
                  else acc @ [ append_path_edge path e ])
                g u [] in
            search_bfs (nqueue @ npaths) acc)) in
    let paths =
      try PG.fold_succ_e (fun e acc -> acc @ [ PG.mk_path [ e ] ]) g src []
      with _ -> [] in
    let res = search_bfs paths [] in
    (* let _ = print "   ...done" in *)
    res
  ;;

  let get_func_vertices_in_pgraph (g : pgraph) : exprs =
    PG.fold_vertex
      (fun v acc ->
        match v with
        | Var u when is_llvalue_function u -> acc @ [ mk_expr_var u ]
        | _ -> acc)
      g []
  ;;

  let find_funcs_of_pointer (prog : program) (g : pgraph) (ptr : llvalue)
      : funcs
    =
    let curr_funcs = get_current_funcs_of_pointer prog ptr in
    let _ = hdebug "Finding functions pointed by: " pr_value ptr in
    (* let _ = hdebug "  - Input: " pr_pgraph g in *)
    let vptr = expr_of_llvalue ptr in
    let vfuncs = get_func_vertices_in_pgraph g in
    let _ = hdebug "Functions in pgraph: " pr_exprs vfuncs in
    let funcs =
      List.fold_left
        ~f:(fun acc v ->
          if check_alias prog g vptr v May
          then (
            (* if check_alias prog g vptr v Must then *)
            match v with
            | Var u ->
              if is_llvalue_function u
              then (
                let func = mk_func u in
                if List.mem acc func ~equal:equal_func
                then acc
                else acc @ [ mk_func u ])
              else acc
            | _ -> acc)
          else acc)
        ~init:curr_funcs vfuncs in
    let _ = hdebug "  Output function pointers: " func_names funcs in
    let _ = update_funcs_of_pointer prog ptr funcs in
    funcs
  ;;

  let is_may_alias_exp (prog : program) (g : pgraph) (u : expr) (v : expr)
      : bool
    =
    check_alias prog g u v May
  ;;

  let is_must_alias_exp (prog : program) (g : pgraph) (u : expr) (v : expr)
      : bool
    =
    check_alias prog g u v Must
  ;;

  let is_no_alias_exp (prog : program) (g : pgraph) (u : expr) (v : expr)
      : bool
    =
    not (is_may_alias_exp prog g u v || is_must_alias_exp prog g u v)
  ;;

  (**
     NOTE:
     - There is a case needs to be considered: what if there is a
       `must-alias` edge which appears in one graph but does not appear
       in the other graph.
       What should be the alias type in the output graph?

     FIXME: need to ensure the correctness of this merging algorithm
  *)

  (* FIXME: it's more like a join operator *)
  let merge_pgraph (g1 : pgraph) (g2 : pgraph) : pgraph =
    let g = PG.create () in
    (* merge edge from the graph g1 first *)
    let _ =
      PG.iter_edges_e
        (fun e1 ->
          let src, dst, lbl1 = PE.src e1, PE.dst e1, PE.label e1 in
          let es2 = PG.find_all_edges g2 src dst in
          let lbls2 = List.map ~f:PE.label es2 in
          (* merge labels of common pattern, if possible *)
          let merged_label =
            List.fold_left
              ~f:(fun acc lbl2 ->
                match acc with
                | Some _ -> acc
                | None ->
                  (match PG.merge_label lbl1 lbl2 with
                  | None -> None
                  | Some lbl -> Some lbl))
              ~init:None lbls2 in
          match merged_label with
          | Some lbl -> PG.add_edge_e g (PE.create src lbl dst)
          | None ->
            List.iter
              ~f:(fun lbl ->
                let lbl = PG.set_precision May lbl in
                (* let lbl = PG.update_label_trace_with_blocks lbl blks1 in *)
                PG.add_edge_e g (PE.create src lbl dst))
              (lbl1 :: lbls2))
        g1 in
    (* now, merge edge from the graph g2 *)
    let _ =
      PG.iter_edges_e
        (fun e2 ->
          let src, dst, lbl = PE.src e2, PE.dst e2, PE.label e2 in
          if not (PG.mem_edge g src dst)
          then (
            let lbl = PG.set_precision May lbl in
            (* let lbl = PG.update_label_trace_with_blocks lbl blks2 in *)
            PG.add_edge_e g (PE.create src lbl dst)))
        g2 in
    g
  ;;

  let join_pgraph g1 g2 : pgraph =
    let g = PG.copy g1 in
    let _ =
      PG.iter_edges_e
        (fun e ->
          let src, lbl, dst = PE.src e, PE.label e, PE.dst e in
          let edges = PG.find_all_edges g src dst in
          let nlbl, merged_edges =
            List.fold_left
              ~f:(fun (accl, acce) edge ->
                let l = PE.label edge in
                match PG.merge_label accl l with
                | None -> accl, acce
                | Some naccl -> naccl, acce @ [ edge ])
              ~init:(lbl, []) edges in
          if List.is_empty merged_edges
          then PG.add_edge_e g e
          else (
            let _ = List.iter ~f:(PG.remove_edge_e g) merged_edges in
            PG.add_edge_e g (PE.create src nlbl dst)))
        g2 in
    g
  ;;
end

(*******************************************************************
 ** Core data transfer modules
 *******************************************************************)

module PointerData = struct
  type t = PointerDomain.pgraph
end

module PointerTransfer : DF.ForwardDataTransfer with type t = PointerData.t =
struct
  open PointerDomain
  include PointerData
  include DF.MakeDefaultEnv (PointerData)

  let analysis = DfaPointer

  (*******************************************************************
   ** Handling atstract data
   *******************************************************************)

  (* constants *)

  let least_data = PointerDomain.least_graph

  (* printers *)

  let pr_data (g : t) = pr_pgraph g
  let pr_data_checksum (g : t) = pr_graph_size g

  (* comparison *)

  let equal_data (d1 : t) (d2 : t) : bool =
    (* TODO: IMPLEMENT *)
    PointerDomain.equal_pgraph d1 d2
  ;;

  let lequal_data = lequal_pgraph

  (* handling data *)

  let copy_data d = PG.copy d

  let subst_data
      ?(sstv : substv = [])
      ?(sstve : substve = [])
      ?(sste : subste = [])
      (g : t)
      : t
    =
    subst_pgraph ~sstv ~sstve ~sste g
  ;;

  let merge_data ?(widen = false) (g1 : t) (g2 : t) : t = merge_pgraph g1 g2

  let subtract_data (g1 : t) (g2 : t) : t =
    (* FIXME: need to implement later *)
    g1
  ;;

  let join_data (g1 : t) (g2 : t) : t = join_pgraph g1 g2

  let free_pointer_from_pgraph (prog : program) (g : t) (v : expr) : t =
    let ng = PG.copy g in
    let aliases = find_all_aliases prog ng v Must in
    let _ = List.iter ~f:(PG.remove_vertex ng) aliases in
    ng
  ;;

  let remove_deref_of_vertex ?(trace_skip = []) (input : t) (v : expr) : unit =
    (* let _ = hdebug ~indent:4 "Remove deref vertex: " pr_expr v in *)
    let vderef = deref_of_expr v in
    let removeable_edges =
      try
        PG.fold_edges_e
          (fun e acc ->
            let src, lbl, dst = PE.src e, PE.label e, PE.dst e in
            match lbl with
            | PG.Alias _ ->
              if equal_expr vderef src || equal_expr vderef dst
              then acc @ [ e ]
              else acc
            | Pto d ->
              if d == To
              then
                if (equal_expr src v && not (equal_expr dst vderef))
                   || (equal_expr dst vderef && not (equal_expr src v))
                then acc @ [ e ]
                else acc
              else if (equal_expr src vderef && not (equal_expr dst v))
                      || (equal_expr dst v && not (equal_expr src vderef))
              then acc @ [ e ]
              else acc
            | GEP (_, _, d) ->
              if d == To && equal_expr dst vderef
              then acc @ [ e ]
              else if d == From && equal_expr src vderef
              then acc @ [ e ]
              else acc
            | Seq (lbls, _, _) when List.not_empty lbls ->
              let first_lbl, last_lbl = List.hd_exn lbls, List.last_exn lbls in
              (match first_lbl, last_lbl with
              | Pto To, _ ->
                if (equal_expr src v && not (equal_expr dst vderef))
                   || (equal_expr dst vderef && not (equal_expr src v))
                then acc @ [ e ]
                else acc
              | GEP (_, _, To), _ ->
                if equal_expr dst vderef then acc @ [ e ] else acc
              | _, Pto From ->
                if (equal_expr src vderef && not (equal_expr dst v))
                   || (equal_expr dst v && not (equal_expr src vderef))
                then acc @ [ e ]
                else acc
              | _, GEP (_, _, From) ->
                if equal_expr src vderef then acc @ [ e ] else acc
              | _ -> acc)
            | _ -> acc)
          input []
      with _ -> [] in
    List.iter
      ~f:(fun e ->
        if (not !dfa_path_sensitive) || List.is_empty trace_skip
        then
          (* let _ = hdebug "  remove edge: " PG.pr_edge e in *)
          PG.remove_edge_e input e
        else (
          let src, dst, lbl = PE.src e, PE.dst e, PE.label e in
          let t = PG.get_trace lbl in
          let nt = PG.update_trace_with_skip_items t trace_skip in
          (* let _ = hdebug "update NotAlias to edge: " PG.pr_edge e in *)
          let nlbl = PG.set_trace nt lbl in
          let _ = PG.remove_edge_e input e in
          PG.add_edge_e input (PE.create src nlbl dst)))
      removeable_edges
  ;;

  let is_data_satisfied_predicate (d : t) (p : predicate) : bool = true
  let refine_data_by_predicate ?(widen = false) (g : t) (p : predicate) : t = g

  let simplify_function_pointer_edges (g : t) : unit =
    let vertices =
      PG.fold_vertex
        (fun v acc ->
          match v with
          | Var vv when is_llvalue_function vv ->
            (* ||
             * is_llvalue_param vv ||
             * is_llvalue_global vv -> *)
            v :: acc
          | _ -> acc)
        g [] in
    List.iter
      ~f:(fun v ->
        let compare_edge e1 e2 =
          match PE.label e1, PE.label e2 with
          | PG.Seq (lbls1, _, _), PG.Seq (lbls2, _, _) ->
            let len1, len2 = List.length lbls1, List.length lbls2 in
            if len1 < len2 then -1 else if len1 > len2 then 1 else 0
          | _ -> 0 in
        let succ_seq_edges =
          let edges =
            PG.fold_succ_e
              (fun e acc ->
                match PE.label e with
                | PG.Seq _ -> e :: acc
                | _ -> acc)
              g v [] in
          List.sorti ~compare:compare_edge edges in
        let pred_seq_edges =
          let edges =
            PG.fold_pred_e
              (fun e acc ->
                match PE.label e with
                | PG.Seq _ -> e :: acc
                | _ -> acc)
              g v [] in
          List.sorti ~compare:compare_edge edges in
        let rec remove_edges edges =
          match edges with
          | [] -> ()
          | e :: nedges ->
            let edges1, edges2 =
              List.partition_tf
                ~f:(fun e2 ->
                  match PE.label e, PE.label e2 with
                  | PG.Seq (lbls1, _, _), PG.Seq (lbls2, _, _) ->
                    List.length lbls1 < List.length lbls2
                    && List.is_prefix lbls2 ~prefix:lbls1 ~equal:PG.equal_label
                  | _ -> false)
                nedges in
            let _ = List.iter ~f:(fun e2 -> PG.remove_edge_e g e2) edges1 in
            remove_edges edges2 in
        let _ = remove_edges pred_seq_edges in
        remove_edges succ_seq_edges)
      vertices
  ;;

  let is_irrelevant_var penv (f : func) (v : llvalue) =
    match LL.classify_value v with
    | LV.Instruction _ -> true (* local var *)
    | LV.GlobalVariable ->
      (* FIXME: need better heuristics to handle global vars *)
      if !dfa_used_globals_selective
      then (
        let used_globals = f |> get_sparse_used_globals penv in
        not (List.mem used_globals (mk_global v) ~equal:equal_global))
      else false
    | LV.Argument ->
      let fparent = LL.param_parent v in
      not (equal_func f (mk_func fparent))
    | _ -> false
  ;;

  let is_removeable_vertex penv func (e : expr) =
    let rec root_llvalue_of_vertex (e : expr) =
      match e with
      | Undef _ | Int64 _ | Float _ | String _ -> []
      | Malloc e -> []
      | Var v -> [ v ]
      | OldE e -> root_llvalue_of_vertex e
      | Deref e -> root_llvalue_of_vertex e
      | ElemPtr (e, _, _) -> root_llvalue_of_vertex e
      | FuncRes f -> [ llvalue_of_func f ]
      | Exn e -> root_llvalue_of_vertex e in
    match e with
    | OldE _ -> true
    | _ ->
      let vs = root_llvalue_of_vertex e in
      List.exists ~f:(fun v -> is_irrelevant_var penv func v) vs
  ;;

  let is_candidate_replacer_vertex (e : expr) =
    match e with
    | Var _ | Deref _ | ElemPtr _ | FuncRes _ -> true
    | _ -> false
  ;;

  let num_pred_edges g v : int = PG.fold_pred_e (fun _ acc -> acc + 1) g v 0
  let num_succ_edges g v : int = PG.fold_succ_e (fun _ acc -> acc + 1) g v 0

  let num_irremovable_neighbor_vertices penv func g v : int =
    let num_succ_vertices =
      PG.fold_succ
        (fun u acc ->
          if is_removeable_vertex penv func u then acc else acc + 1)
        g v 0 in
    let num_pred_vertices =
      PG.fold_pred
        (fun u acc ->
          if is_removeable_vertex penv func u then acc else acc + 1)
        g v 0 in
    num_pred_vertices + num_succ_vertices
  ;;

  (** Eliminate alias edges of inner vertices *)

  let remove_must_alias_vertices penv func (g : pgraph) : unit =
    let find_removable_vertex g : (llvalue * expr) option =
      let res =
        PG.fold_edges_e
          (fun e acc ->
            match acc with
            | None ->
              if PG.is_must_alias_edge e
              then (
                let src, dst = PE.src e, PE.dst e in
                if is_removeable_vertex penv func src
                   && is_candidate_replacer_vertex dst
                   && not (is_removeable_vertex penv func dst)
                then (
                  match src with
                  | Var vsrc -> Some (vsrc, dst)
                  | _ -> None)
                else if is_removeable_vertex penv func dst
                        && is_candidate_replacer_vertex src
                        && not (is_removeable_vertex penv func src)
                then (
                  match dst with
                  | Var vdst -> Some (vdst, src)
                  | _ -> None)
                else acc)
              else acc
            | Some _ -> acc)
          g None in
      match res with
      | Some _ -> res
      | None ->
        PG.fold_edges_e
          (fun e acc ->
            match acc with
            | None ->
              if PG.is_must_alias_edge e
              then (
                let src, dst = PE.src e, PE.dst e in
                if is_removeable_vertex penv func src
                   && is_candidate_replacer_vertex dst
                then (
                  match src with
                  | Var vsrc -> Some (vsrc, dst)
                  | _ -> None)
                else if is_removeable_vertex penv func dst
                        && is_candidate_replacer_vertex src
                then (
                  match dst with
                  | Var vdst -> Some (vdst, src)
                  | _ -> None)
                else None)
              else None
            | Some _ -> acc)
          g None in
    let rec remove_must_alias g =
      match find_removable_vertex g with
      | None -> ()
      | Some (vreplacee, replacer) ->
        (* let _ = debug ("Replace must_alias: " ^ (pr_value vreplacee) ^ " by " ^
         *                (pr_expr replacer)) in *)
        let replacee = mk_expr_var vreplacee in
        let sstve = mk_substve ~oldvs:[ vreplacee ] ~newes:[ replacer ] in
        let removed_edges, new_edges =
          PG.fold_edges_e
            (fun e (accr, accn) ->
              let src, dst, lbl = PE.src e, PE.dst e, PE.label e in
              let nsrc, ndst = subst_expr ~sstve src, subst_expr ~sstve dst in
              if equal_expr src nsrc && equal_expr dst ndst
              then accr, accn
              else if PG.is_must_alias_edge e
                      && ((equal_expr src replacee && equal_expr dst replacer)
                         || (equal_expr src replacer && equal_expr dst replacee)
                         )
              then accr @ [ e ], accn
              else (
                let ne = PE.create nsrc lbl ndst in
                accr @ [ e ], accn @ [ ne ]))
            g ([], []) in
        let _ =
          List.iter
            ~f:(fun e ->
              let _ = PG.remove_edge_e g e in
              let src, dst = PE.src e, PE.dst e in
              let _ =
                if num_pred_edges g src = 0 && num_succ_edges g src = 0
                then PG.remove_vertex g src in
              let _ =
                if num_pred_edges g dst = 0 && num_succ_edges g dst = 0
                then PG.remove_vertex g dst in
              ())
            removed_edges in
        let _ = List.iter ~f:(fun e -> PG.add_edge_e g e) new_edges in
        remove_must_alias g in
    remove_must_alias g
  ;;

  let compare_removable_vs penv func g v1 v2 =
    let has_unused_joined_edge v =
      let succ_vtxs =
        PG.fold_succ
          (fun u acc ->
            if is_removeable_vertex penv func u then acc else acc @ [ u ])
          g v [] in
      let pred_vtxs =
        PG.fold_pred
          (fun u acc ->
            if is_removeable_vertex penv func u then acc else acc @ [ u ])
          g v [] in
      List.exists
        ~f:(fun u1 ->
          List.exists
            ~f:(fun u2 ->
              (is_sub_expr v ~sub:u1 && is_sub_expr v ~sub:u2)
              || (is_sub_expr u1 ~sub:v && is_sub_expr u2 ~sub:v))
            succ_vtxs)
        pred_vtxs in
    let num_pred_1, num_succ_1 = num_pred_edges g v1, num_succ_edges g v1 in
    let num_pred_2, num_succ_2 = num_pred_edges g v2, num_succ_edges g v2 in
    let num_new_edges_1 = num_pred_1 * num_succ_1 in
    let num_new_edges_2 = num_pred_2 * num_succ_2 in
    if num_new_edges_1 < num_new_edges_2
    then -1
    else if num_new_edges_1 > num_new_edges_2
    then 1
    else (
      let is_internal_vtx_1 = is_expr_elemptr v1 || is_expr_deref v1 in
      let is_internal_vtx_2 = is_expr_elemptr v2 || is_expr_deref v2 in
      if is_internal_vtx_1 && not is_internal_vtx_2
      then -1
      else if (not is_internal_vtx_1) && is_internal_vtx_2
      then 1
      else (
        let has_unused_edge_1 = has_unused_joined_edge v1 in
        let has_unused_edge_2 = has_unused_joined_edge v2 in
        if (not has_unused_edge_1) && has_unused_edge_2
        then -1
        else if has_unused_edge_1 && not has_unused_edge_2
        then 1
        else 0))
  ;;

  (** Eliminate alias edges of inner vertices *)

  let eliminate_alias_edges_by_join_edges penv func (g : pgraph) : unit =
    let removable_vs =
      PG.fold_vertex
        (fun v acc ->
          if is_removeable_vertex penv func v then acc @ [ v ] else acc)
        g [] in
    let removable_vs =
      List.sorti ~compare:(compare_removable_vs penv func g) removable_vs in
    let rec remove_vertices vs =
      match vs with
      | [] -> ()
      | v :: nvs ->
        let pred_edges = PG.fold_pred_e (fun e acc -> e :: acc) g v [] in
        let succ_edges = PG.fold_succ_e (fun e acc -> e :: acc) g v [] in
        let _ =
          List.iter
            ~f:(fun e1 ->
              List.iter
                ~f:(fun e2 ->
                  let src, dst = PE.src e1, PE.dst e2 in
                  let lbl1, lbl2 = PE.label e1, PE.label e2 in
                  if (not
                        (equal_expr src dst || is_expr_undef src
                       || is_expr_undef dst || is_expr_null src
                       || is_expr_null dst
                        || (is_expr_function src && is_expr_function dst)
                        || (is_expr_function src && is_expr_malloc dst)
                        || (is_expr_malloc src && is_expr_function dst)))
                     && (PG.is_alias_edge e1 || PG.is_alias_edge e2)
                  then (
                    match PG.fold_label lbl1 lbl2 with
                    | None -> ()
                    | Some nlbl ->
                      let edge = PE.create src nlbl dst in
                      let _ =
                        if PG.is_alias_edge e1 then PG.remove_edge_e g e1 in
                      (* let _ = hdebug "  remove edge: " PG.pr_edge e1 in *)
                      let _ =
                        if PG.is_alias_edge e2 then PG.remove_edge_e g e2 in
                      (* let _ = hdebug "  remove edge: " PG.pr_edge e2 in *)
                      let current_edges = PG.find_all_edges g src dst in
                      let has_edge_of_similar_label =
                        List.exists
                          ~f:(fun e ->
                            let lble = PE.label e in
                            PG.check_equiv_label nlbl lble
                            || PG.check_suffix_seq_label nlbl lble
                            || PG.check_prefix_seq_label nlbl lble)
                          current_edges in
                      if has_edge_of_similar_label
                         || ((is_sub_expr src ~sub:dst
                            || is_sub_expr dst ~sub:src)
                            && not (is_structural_access_edge edge))
                      then ()
                      else PG.add_edge_e g edge)
                  else ())
                succ_edges)
            pred_edges in
        remove_vertices nvs in
    remove_vertices removable_vs
  ;;

  (** Eliminate an inner vertex by joining its predecessors with
      its successors by multi-label edges. *)

  let eliminate_vertex_by_join_edges (g : pgraph) ?(kept_vs = None) (v : expr)
      : unit
    =
    let connected_kept_vs =
      match kept_vs with
      | None -> []
      | Some kept_vs ->
        (* let _ = hdebug "kept_vs: " pr_exprs kept_vs in *)
        let connected_vs = find_connected_vertices g v in
        List.filter
          ~f:(fun u -> List.mem kept_vs u ~equal:equal_expr)
          connected_vs in
    let new_edges =
      if kept_vs != None && List.length connected_kept_vs < 2
      then []
      else (
        let compare_edge_length e1 e2 =
          let len1, len2 =
            PG.get_edge_label_length e1, PG.get_edge_label_length e2 in
          if len1 < len2 then -1 else if len1 > len2 then 1 else 0 in
        let remove_equiv_or_prefix_suffix_edges edges =
          List.fold_left
            ~f:(fun acc e ->
              let src, lbl, dst = PE.src e, PE.label e, PE.dst e in
              if List.exists
                   ~f:(fun e2 ->
                     let src2, lbl2, dst2 = PE.src e2, PE.label e2, PE.dst e2 in
                     equal_expr src2 src && equal_expr dst2 dst
                     && (PG.check_equiv_label lbl2 lbl
                        || PG.check_suffix_seq_label lbl2 lbl
                        || PG.check_prefix_seq_label lbl2 lbl))
                   acc
              then acc
              else acc @ [ e ])
            ~init:[] edges in
        let pred_edges =
          let edges = PG.fold_pred_e (fun e acc -> e :: acc) g v [] in
          let edges = List.sorti ~compare:compare_edge_length edges in
          remove_equiv_or_prefix_suffix_edges edges in
        let succ_edges =
          let edges = PG.fold_succ_e (fun e acc -> e :: acc) g v [] in
          let edges = List.sorti ~compare:compare_edge_length edges in
          remove_equiv_or_prefix_suffix_edges edges in
        List.fold_left
          ~f:(fun acc1 e1 ->
            List.fold_left
              ~f:(fun acc2 e2 ->
                let src, dst = PE.src e1, PE.dst e2 in
                let lbl1, lbl2 = PE.label e1, PE.label e2 in
                let current_edges =
                  PG.find_all_edges g src dst
                  @ List.filter
                      ~f:(fun e ->
                        equal_expr src (PE.src e) && equal_expr dst (PE.dst e))
                      acc2 in
                if equal_expr src dst || equal_expr v dst
                then acc2
                else if is_expr_undef src || is_expr_undef dst
                        || is_expr_null src || is_expr_null dst
                then acc2
                else if (is_expr_function src && is_expr_function dst)
                        || (is_expr_function src && is_expr_malloc dst)
                        || (is_expr_malloc src && is_expr_function dst)
                then
                  acc2
                  (* else if has_must_alias_edges g src dst then acc2 *)
                else if List.exists ~f:PG.is_alias_edge current_edges
                then acc2
                else (
                  match PG.fold_label lbl1 lbl2 with
                  | None -> acc2
                  | Some nlbl ->
                    let edge = PE.create src nlbl dst in
                    (* NOTE: the heuristics of checking different directions
                             affect the benchmark CPP
                             TODO: need to improve the conditions on direction checking
                             so that it is both precise and scalable *)
                    if PG.has_consecutive_edges_of_direction_to_from nlbl
                    then acc2
                    else if (is_sub_expr src ~sub:dst
                           || is_sub_expr dst ~sub:src)
                            && not (is_structural_access_edge edge)
                    then acc2
                    else if List.exists
                              ~f:(fun e ->
                                let lble = PE.label e in
                                PG.check_equiv_label nlbl lble
                                || PG.check_suffix_seq_label nlbl lble
                                || PG.check_prefix_seq_label nlbl lble)
                              current_edges
                    then acc2
                    else acc2 @ [ edge ]))
              ~init:acc1 succ_edges)
          ~init:[] pred_edges) in
    let _ =
      if List.not_empty new_edges then List.iter ~f:(PG.add_edge_e g) new_edges
    in
    PG.remove_vertex g v
  ;;

  let join_edges_of_irrelevant_vars penv func (g : pgraph) : unit =
    (* first, remove aliases by substitution *)
    (* let _ = hdebug "Before joining edges: " pr_data g in *)
    let _ = remove_must_alias_vertices penv func g in
    (* let _ = hdebug "After remove must-alias: " pr_data g in *)
    (* second, remove alias by joining edges *)
    let _ = eliminate_alias_edges_by_join_edges penv func g in
    (* let _ = hdebug "After join alias edges: " pr_data g in *)
    (* finally, fold other edges *)
    let removable_vs, kept_vs =
      PG.fold_vertex
        (fun v (accr, acck) ->
          if is_removeable_vertex penv func v
          then accr @ [ v ], acck
          else accr, acck @ [ v ])
        g ([], []) in
    (* let _ = hdebug "removable_vs: " pr_exprs removable_vs in *)
    if List.is_empty kept_vs
    then List.iter ~f:(PG.remove_vertex g) removable_vs
    else (
      let rvs =
        List.sorti ~compare:(compare_removable_vs penv func g) removable_vs
      in
      List.iter
        ~f:(fun v ->
          let _ = eliminate_vertex_by_join_edges g ~kept_vs:(Some kept_vs) v in
          ())
        rvs)
  ;;

  let join_edges_of_internal_vertices penv func (g : pgraph) : unit =
    let removable_vs =
      PG.fold_vertex
        (fun v acc ->
          match v with
          | Deref _ | ElemPtr _ ->
            let succ_vs = PG.fold_succ (fun v acc -> v :: acc) g v [] in
            let pred_vs = PG.fold_pred (fun v acc -> v :: acc) g v [] in
            let connected_vs = succ_vs @ pred_vs in
            if List.exists ~f:(fun u -> is_sub_expr v ~sub:u) connected_vs
               && List.exists ~f:(fun u -> is_sub_expr u ~sub:v) connected_vs
            then acc @ [ v ]
            else acc
          | _ -> acc)
        g [] in
    List.iter
      ~f:(fun v ->
        let _ = eliminate_vertex_by_join_edges g ~kept_vs:None v in
        ())
      removable_vs
  ;;

  let remove_removable_vertex penv func (g : pgraph) : unit =
    let removable_vs =
      PG.fold_vertex
        (fun v acc ->
          if is_removeable_vertex penv func v then acc @ [ v ] else acc)
        g [] in
    List.iter ~f:(PG.remove_vertex g) removable_vs
  ;;

  let remove_non_edges_vertices g : unit =
    let vs =
      PG.fold_vertex
        (fun v acc ->
          let has_succ_edges =
            try
              let _ = PG.iter_succ (fun _ -> raise (EBool true)) g v in
              false
            with EBool res -> res in
          let has_pred_edges =
            try
              let _ = PG.iter_pred (fun _ -> raise (EBool true)) g v in
              false
            with EBool res -> res in
          if (not has_succ_edges) && not has_pred_edges
          then acc @ [ v ]
          else acc)
        g [] in
    List.iter ~f:(fun v -> PG.remove_vertex g v) vs
  ;;

  let remove_trivial_vertices g : unit =
    let removable_vs =
      PG.fold_vertex
        (fun v acc ->
          match v with
          | Var _ when is_expr_null v -> acc @ [ v ]
          | Deref u when is_expr_null u -> acc @ [ v ]
          | _ -> acc)
        g [] in
    List.iter ~f:(fun v -> PG.remove_vertex g v) removable_vs
  ;;

  let remove_edge_contain_local_var_expr ?(remove = false) (g : t) : unit =
    let rec has_label_containing_var_idx lbl : bool =
      match lbl with
      | PG.GEP (_, idxs, _) ->
        List.exists
          ~f:(fun id ->
            match id with
            | Var v -> is_llvalue_instr v
            | _ -> false)
          idxs
      | PG.Seq (lbls, _, _) -> List.exists ~f:has_label_containing_var_idx lbls
      | _ -> false in
    let removable_edges =
      PG.fold_edges_e
        (fun e acc ->
          if has_label_containing_var_idx (PE.label e)
          then acc @ [ e ]
          else acc)
        g [] in
    List.iter ~f:(PG.remove_edge_e g) removable_edges
  ;;

  let generalize_var_expr ?(remove = false) (g : t) : unit =
    let generalize_vertex v : expr * bool =
      match v with
      | Malloc e when is_expr_var e ->
        let nv = Malloc (mk_expr_undef (type_of_expr e)) in
        nv, true
      | _ -> v, false in
    let rec generalize_label lbl : PG.label * bool =
      match lbl with
      | PG.GEP (t, idxs, d) when List.exists ~f:is_expr_var idxs ->
        let nidxs =
          List.map
            ~f:(fun idx ->
              if is_expr_var idx then mk_expr_undef (type_of_expr idx) else idx)
            idxs in
        PG.GEP (t, nidxs, d), true
      | PG.Seq (lbls, p, t) ->
        let nlbls, updated =
          List.fold_left
            ~f:(fun (accl, accu) lbl ->
              let nlbl, updated = generalize_label lbl in
              accl @ [ nlbl ], accu || updated)
            ~init:([], false) lbls in
        PG.Seq (nlbls, p, t), updated
      | _ -> lbl, false in
    let old_edges, new_edges =
      PG.fold_edges_e
        (fun e (acco, accn) ->
          let src, lbl, dst = PE.src e, PE.label e, PE.dst e in
          let src, src_updated = generalize_vertex src in
          let dst, dst_updated = generalize_vertex dst in
          let lbl, lbl_updated = generalize_label lbl in
          if src_updated || dst_updated || lbl_updated
          then (
            let ne = PE.create src lbl dst in
            acco @ [ e ], accn @ [ ne ])
          else acco, accn)
        g ([], []) in
    let _ = List.iter ~f:(PG.remove_edge_e g) old_edges in
    let _ = if not remove then List.iter ~f:(PG.add_edge_e g) new_edges in
    ()
  ;;

  let remove_unnecessary_structural_edges (g : t) : unit =
    let find_removable_edges () =
      PG.fold_vertex
        (fun v acc ->
          match v with
          | Deref u | ElemPtr (u, _, _) ->
            let succ_edges = PG.fold_succ_e (fun e acc2 -> e :: acc2) g v [] in
            let pred_edges = PG.fold_pred_e (fun e acc2 -> e :: acc2) g v [] in
            let has_only_structural_edges =
              List.for_all
                ~f:(fun e ->
                  is_structural_access_edge e && equal_expr u (PE.dst e))
                succ_edges
              && List.for_all
                   ~f:(fun e ->
                     is_structural_access_edge e && equal_expr u (PE.src e))
                   pred_edges in
            if has_only_structural_edges
            then acc @ succ_edges @ pred_edges
            else acc
          | _ -> acc)
        g [] in
    let removed = ref true in
    while !removed do
      let _ = removed := false in
      let removable_edges = find_removable_edges () in
      if List.not_empty removable_edges
      then (
        let _ = removed := true in
        List.iter ~f:(PG.remove_edge_e g) removable_edges)
      else ()
    done
  ;;

  let remove_duplicated_alias_edges (g : t) : unit =
    let removable_edges =
      PG.fold_vertex
        (fun v acc ->
          let may_edges, must_edges =
            PG.fold_succ_e
              (fun e (amay, amust) ->
                if PG.is_may_alias_edge e
                then amay @ [ e ], amust
                else if PG.is_must_alias_edge e
                then amay, amust @ [ e ]
                else amay, amust)
              g v ([], []) in
          let removable_may_edges =
            List.filter
              ~f:(fun e ->
                List.exists
                  ~f:(fun e2 -> equal_expr (PE.dst e) (PE.dst e2))
                  must_edges)
              may_edges in
          acc @ removable_may_edges)
        g [] in
    List.iter ~f:(PG.remove_edge_e g) removable_edges
  ;;

  let clean_irrelevant_info_from_data penv func (g : t) : pgraph =
    (* let _ = hdebug "Clean irrelevant info from data from: \n" pr_pgraph g in *)
    let ng = PG.copy g in
    let _ = join_edges_of_irrelevant_vars penv func ng in
    (* let _ = join_edges_of_internal_vertices penv func ng in *)
    (* let _ = hdebug "after join edges:\n" pr_pgraph ng in *)
    (* let _ = generalize_var_expr ~remove:true ng in *)
    let _ = generalize_var_expr ~remove:true ng in
    (* let _ = hdebug "after generalize vars:\n" pr_pgraph ng in *)
    (* let _ = hdebug "after remove trivial vertices:\n" pr_pgraph ng in *)
    let _ = remove_non_edges_vertices ng in
    let _ = remove_removable_vertex penv func ng in
    let _ = remove_trivial_vertices ng in
    (* let _ = hprint ~always:true "after remove vertices:\n" pr_pgraph ng in *)
    let _ = remove_unnecessary_structural_edges ng in
    (* let _ = hdebug "after remove trivial deref:\n" pr_pgraph ng in *)
    let _ = remove_duplicated_alias_edges ng in
    (* let _ = hdebug "after remove duplicated alias edges:\n" pr_pgraph ng in *)
    (* let _ = hdebug "after remove duplicated non-edges vertices:\n" pr_pgraph ng in *)
    let _ = remove_non_edges_vertices ng in
    let _ = remove_removable_vertex penv func ng in
    let _ = remove_trivial_vertices ng in
    (* let _ = remove_edge_contain_local_var_expr ng in *)
    (* let ng = connect_sub_vertices ng in *)
    (* let _ = hdebug "after connecting sub exprs:\n" pr_pgraph ng in *)
    (* let _ = simplify_function_pointer_edges ng in *)
    (* let _ = print "   ...done" in *)
    (* let _ = hdebug "Result after cleaning:\n" pr_pgraph ng in *)
    ng
  ;;

  let clean_info_of_vars (input : t) (vs : llvalues) : t =
    let _ =
      List.iter
        ~f:(fun v ->
          let ev = get_bitcast_alias_of_llvalue v in
          let _ = hdebug "Remove vertex: " pr_expr ev in
          PG.remove_vertex input ev)
        vs in
    input
  ;;

  (* sparse analysis *)

  let is_candidate_sparse_instr penv instr : bool =
    match instr_opcode instr with
    | LO.Alloca
    | LO.Br
    | LO.IndirectBr
    | LO.Switch
    | LO.GetElementPtr
    | LO.BitCast
    | LO.Ret -> true
    | LO.LandingPad | LO.Resume -> false (* exception *)
    | LO.Unreachable -> false
    | LO.Store -> is_llvalue_contain_pointer (src_of_instr_store instr)
    | LO.Load -> is_llvalue_contain_pointer (dst_of_instr_load instr)
    | LO.InsertValue ->
      is_llvalue_contain_pointer (src_of_instr_insertvalue instr)
    | LO.ExtractValue ->
      is_llvalue_contain_pointer (dst_of_instr_extractvalue instr)
    | LO.PHI -> is_llvalue_contain_pointer (dst_of_instr_phi instr)
    | LO.Call | LO.Invoke ->
      let callee = callee_of_instr_func_call instr in
      let is_pointer_library_func func =
        let candidate_funcs =
          [ is_discover_assertion_func;
            is_func_malloc;
            is_func_free;
            is_func_cpp_new;
            is_func_cpp_delete;
            is_func_memcpy;
            is_func_memmove;
            is_func_clang_call_terminate;
            is_func_dynamic_cast
            (* is_func_handling_exception *)
          ] in
        List.exists ~f:(fun f -> f func) candidate_funcs in
      is_func_pointer callee
      || is_pointer_library_func callee
      || List.exists ~f:(equal_func callee) penv.penv_goal_funcs
    | _ -> false
  ;;

  let is_global_used_only_in_init_sparse_functions penv (g : global) : bool =
    let vg = llvalue_of_global g in
    (* let _ = hprint "check global: " pr_global g in *)
    let users = get_users vg in
    List.for_all
      ~f:(fun u ->
        match LL.classify_value u with
        | LV.Instruction LO.Store | LV.Instruction LO.GetElementPtr ->
          let func = func_of_instr (mk_instr u) in
          (* let _ = hprint "   func: " func_name func in *)
          is_init_func func
        | _ -> false)
      users
  ;;

  let initialize_candidate_sparse_globals penv : unit =
    let is_global_string (g : global) =
      let gtyp = type_of_global g in
      match LL.classify_type gtyp with
      | LT.Pointer -> is_type_string (LL.element_type gtyp)
      | _ -> false in
    List.iter
      ~f:(fun g ->
        let vg = llvalue_of_global g in
        if is_global_string g
        then
          (* || is_global_used_only_in_init_sparse_functions penv g then *)
          Hashtbl.set penv.penv_sparse_llvalue ~key:vg ~data:false
        else Hashtbl.set penv.penv_sparse_llvalue ~key:vg ~data:true)
      penv.penv_prog.prog_globals
  ;;

  let refine_candidate_sparse_globals penv : bool =
    (* let _ = print "REFINE_CANDIDATE_SPARSE_GLOBALS" in *)
    let updated = ref false in
    let is_used_global g =
      try
        let vg = llvalue_of_global g in
        let _ =
          LL.iter_uses
            (fun u ->
              let vu = LL.user u in
              match LL.classify_value vu with
              | LV.Instruction _ | LV.GlobalVariable _ ->
                if is_sparse_llvalue penv vu then raise (EBool true)
              | _ -> ())
            vg in
        false
      with EBool res -> res in
    let is_global_used_only_as_dst_of_instr_store g =
      try
        let vg = llvalue_of_global g in
        (* let _ = hprint "global: " pr_global g in *)
        let _ =
          LL.iter_uses
            (fun u ->
              let vu = LL.user u in
              if is_sparse_llvalue penv vu
              then (
                (* let _ = hprint "  user: " pr_value_detail vu in *)
                match LL.classify_value vu with
                | LV.Instruction LO.Store ->
                  if equal_value (dst_of_instr_store (mk_instr vu)) vg
                  then ()
                  else raise (EBool false)
                | _ -> raise (EBool false)))
            vg in
        true
      with EBool res -> res in
    let _ =
      List.iter
        ~f:(fun g ->
          if is_sparse_global penv g
          then
            if (not (is_used_global g))
               || is_global_used_only_as_dst_of_instr_store g
            then (
              (* || is_global_used_only_in_init_sparse_functions penv g then *)
              let vg = llvalue_of_global g in
              let _ = updated := true in
              let _ =
                Hashtbl.set penv.penv_sparse_llvalue ~key:vg ~data:false in
              LL.iter_uses
                (fun u ->
                  let vu = LL.user u in
                  Hashtbl.set penv.penv_sparse_llvalue ~key:vu ~data:false)
                vg))
        penv.penv_prog.prog_globals in
    !updated
  ;;

  let initialize_candidate_sparse_instrs penv : unit =
    let visit_instr i =
      let vi = llvalue_of_instr i in
      if is_candidate_sparse_instr penv i
      then Hashtbl.set penv.penv_sparse_llvalue ~key:vi ~data:true
      else Hashtbl.set penv.penv_sparse_llvalue ~key:vi ~data:false in
    iter_struct_program ~finstr:(Some visit_instr) penv.penv_prog
  ;;

  let refine_candidate_sparse_instrs penv : bool =
    let updated = ref false in
    let rec refine_func f : unit =
      (* let _ = hdebug "Refining sparse function: " func_name f in *)
      let continue = ref false in
      let visit_instr i =
        if is_sparse_instr penv i
        then (
          let vi, oprs = llvalue_of_instr i, operands i in
          let num_using_sparse_instrs (v : llvalue) : int =
            LL.fold_left_uses
              (fun acc u ->
                let vu = LL.user u in
                if is_llvalue_instr vu && is_sparse_llvalue penv vu
                then acc + 1
                else acc)
              0 v in
          let is_value_used_only_as_dst_of_instr_store v =
            try
              LL.iter_uses
                (fun u ->
                  let vu = LL.user u in
                  match LL.classify_value vu with
                  | LV.Instruction LO.Store ->
                    if not (is_sparse_llvalue penv vu)
                    then ()
                    else if equal_value (dst_of_instr_store (mk_instr vu)) v
                    then ()
                    else raise (EBool false)
                  | _ -> raise (EBool false))
                v;
              true
            with EBool res -> res in
          let has_operand_of_non_sparse_global (oprs : llvalue list) =
            List.exists
              ~f:(fun opr ->
                is_llvalue_global opr && not (is_sparse_llvalue penv opr))
              oprs in
          let has_operand_of_non_sparse_instr (oprs : llvalue list) =
            List.exists
              ~f:(fun opr ->
                is_llvalue_instr opr && not (is_sparse_llvalue penv opr))
              oprs in
          let can_mark_non_sparse_instr =
            match instr_opcode i with
            | LO.Alloca ->
              num_using_sparse_instrs vi = 0
              || is_value_used_only_as_dst_of_instr_store vi
            | LO.Load | LO.GetElementPtr | LO.BitCast ->
              has_operand_of_non_sparse_global oprs
              || num_using_sparse_instrs vi = 0
            | LO.Store ->
              let src = src_of_instr_store i in
              let dst = dst_of_instr_store i in
              has_operand_of_non_sparse_instr oprs
              || (is_llvalue_global src && is_llvalue_instr src
                 && not (is_sparse_llvalue penv src))
              || (is_llvalue_global dst && is_llvalue_instr dst
                 && not (is_sparse_llvalue penv dst))
            (* || has_operand_of_non_sparse_global oprs *)
            (* not (used_by_other_sparse_instrs (dst_of_instr_store i)) *)
            | _ -> false in
          if can_mark_non_sparse_instr
          then (
            let _ = continue := true in
            let _ = updated := true in
            Hashtbl.set penv.penv_sparse_llvalue ~key:vi ~data:false)
          else ())
        else () in
      let _ = iter_struct_func ~finstr:(Some visit_instr) f in
      if !continue then refine_func f else () in
    let visit_func f =
      let _ = refine_func f in
      Some () in
    let _ = iter_struct_program ~ffunc:(Some visit_func) penv.penv_prog in
    !updated
  ;;

  let refine_further_sparse_candidate_instr penv : unit =
    (* TODO: TO-IMPLEMENT: target to only instruction related to assertion, bug
       checking, etc ...*)
    ()
  ;;

  let init_sparse_globals_instrs penv : unit =
    let _ = initialize_candidate_sparse_globals penv in
    initialize_candidate_sparse_instrs penv
  ;;

  let refine_sparse_globals_instrs penv : bool =
    let updated = ref false in
    let continue = ref true in
    let _ =
      while !continue do
        let _ = continue := false in
        let _ =
          if refine_candidate_sparse_instrs penv
          then (
            updated := true;
            continue := true) in
        if refine_candidate_sparse_globals penv
        then (
          updated := true;
          continue := true)
      done in
    !updated
  ;;

  (*******************************************************************
   ** Core analysis functions
   *******************************************************************)

  let prepare_entry_func_input (penv : prog_env) func (input : t) : t = input

  let prepare_callee_input penv instr callee args (input : t) : t =
    let args = List.map ~f:get_bitcast_alias_of_llvalue args in
    let _ = hdebug ~indent:4 "Prepare callee's input: " func_name callee in
    let _ = hdebug ~indent:4 " - Initial input: " pr_data input in
    let params =
      let params = formal_params_of_func callee in
      List.map
        ~f:(fun p -> get_bitcast_alias_of_llvalue (llvalue_of_param p))
        params in
    let _ = hdebug ~indent:4 " - args: " pr_exprs args in
    let _ = hdebug ~indent:4 " - params: " pr_exprs params in
    (* parameter binding by MustAlias edges between formal and actual args *)
    let output = PG.copy input in
    (* let output =
     *   let output = PG.copy input in
     *   let old_params = List.map ~f:(fun e -> OldE e) params in
     *   let sste = mk_subste ~oldes:params ~newes:old_params in
     *   subst_pgraph ~sste output in *)
    let _ =
      if List.length args == List.length params
      then
        List.iter2_exn
          ~f:(fun arg param ->
            let atyp, ptyp = type_of_expr arg, type_of_expr param in
            if is_type_pointer atyp && is_type_pointer ptyp
            then insert_alias output Must arg param)
          args params
      else
        warning
          ("prepare_callee_input: args and params of different length\n"
         ^ "  - function " ^ func_name callee ^ "\n" ^ "  - instr call: "
         ^ pr_instr instr ^ "\n") in
    (* let output =
     *   let sste = mk_subste ~oldes:args ~newes:params in
     *   subst_data ~sste input in *)
    let _ = hdebug ~indent:4 " - Input with params: " pr_data output in
    (* and then remove all local vars *)
    let output, time =
      Sys.track_runtime (fun () ->
          clean_irrelevant_info_from_data penv callee output) in
    let _ = hdebug "  Final input : " pr_data output in
    output
  ;;

  let refine_caller_data_by_deref_args prog (input : t) (args : exprs) : t =
    let args = List.map ~f:get_bitcast_alias args in
    let output = PG.copy input in
    let _ =
      List.iter
        ~f:(fun arg ->
          let arg_aliases = find_all_aliases prog output arg Must in
          let _ = remove_deref_of_vertex output arg in
          List.iter ~f:(remove_deref_of_vertex output) arg_aliases)
        args in
    output
  ;;

  let refine_callee_output prog instr callee (output : t) : t =
    if !dfa_path_sensitive
    then (
      let noutput = PG.create () in
      let _ =
        PG.iter_edges_e
          (fun e ->
            let src, lbl, dst = PE.src e, PE.label e, PE.dst e in
            let tr = PG.get_trace lbl in
            let t = PG.mk_trace_simple instr in
            let ntr =
              if PG.has_trace_item_of_func tr callee
              then PG.fold_trace tr t
              else tr in
            let nlbl = PG.set_trace ntr (PE.label e) in
            let ne = PE.create src nlbl dst in
            PG.add_edge_e noutput ne)
          output in
      noutput)
    else output
  ;;

  let compute_callee_output_exns penv instr callee args input fsum : t * exns =
    let prog = penv.penv_prog in
    let args = List.map ~f:get_bitcast_alias_of_llvalue args in
    let params =
      let params = formal_params_of_func callee in
      List.map
        ~f:(fun p -> get_bitcast_alias_of_llvalue (llvalue_of_param p))
        params in
    let sste =
      let callee_res = mk_expr_func_result callee in
      let caller_call = expr_of_instr instr in
      mk_subste ~oldes:(callee_res :: params) ~newes:(caller_call :: args)
    in
    let output =
      let dargs =
        let dparams =
          fsum.fsum_deref_params |> llvalues_of_params
          |> List.map ~f:(fun v -> subst_expr ~sste (expr_of_llvalue v)) in
        let dglobals =
          fsum.fsum_deref_globals |> llvalues_of_globals
          |> List.map ~f:(fun v -> subst_expr ~sste (expr_of_llvalue v)) in
        dparams @ dglobals in
      let caller_data = refine_caller_data_by_deref_args prog input dargs in
      let callee_output = subst_data ~sste fsum.fsum_output in
      let callee_output =
        refine_callee_output prog instr callee callee_output in
      join_data caller_data callee_output in
    let exns =
      List.map
        ~f:(fun exn ->
          let ndata = subst_data ~sste exn.exn_data in
          { exn with exn_data = ndata })
        fsum.fsum_thrown_exn in
    output, exns
  ;;

  let prepare_thrown_exception_data penv exn_ptr tinfo input : t =
    let exn_orig_expr = mk_expr_exn tinfo in
    let ptr_expr = mk_expr_var exn_ptr in
    let sste = mk_subste ~oldes:[ ptr_expr ] ~newes:[ exn_orig_expr ] in
    let output = subst_data ~sste input in
    output
  ;;

  let compute_catch_exception_data penv instr ptr input exn : t =
    let src = mk_expr_deref ptr in
    let dst = mk_expr_var (llvalue_of_instr instr) in
    let output = PG.copy input in
    let output =
      let sste =
        mk_subste ~oldes:[ exn.exn_root_expr ] ~newes:[ mk_expr_var ptr ] in
      let exn_data = subst_data ~sste exn.exn_data in
      join_data output exn_data in
    let _ = insert_alias output Must src dst in
    output
  ;;

  let need_widening func : bool = false

  (* analyzing globals and instructions *)

  let analyze_global (g : global) (input : t) : t =
    (* default behavior *)
    let init_value = global_operand g 0 in
    let init_typ = LL.type_of init_value in
    match LL.classify_type init_typ with
    | LT.Void -> input
    | LT.Pointer ->
      if not (LL.is_null init_value)
      then (
        let gexp = expr_of_global g in
        let deref = deref_of_global g in
        let ivexp = expr_of_llvalue init_value in
        let output = PG.copy input in
        let _ = insert_deref output ~deref ~root:gexp in
        let _ = insert_alias output Must deref ivexp in
        output)
      else input
    | LT.Struct ->
      let root = expr_of_llvalue (llvalue_of_global g) in
      let rtyp = type_of_expr root in
      let output = PG.copy input in
      for i = 0 to LL.num_operands init_value do
        let fld_ptr = LL.operand init_value i in
        if is_llvalue_pointer fld_ptr && not (LL.is_null fld_ptr)
        then (
          let fst_idx = expr_of_int32 0 in
          let snd_idx = expr_of_int32 i in
          let fld_exp = expr_of_llvalue fld_ptr in
          let idxs = [ fst_idx; snd_idx ] in
          let elemptr = elemptr_of_global g idxs in
          let _ = insert_deref output ~deref:fld_exp ~root:elemptr in
          insert_element_ptr output ~elemptr ~root ~rtyp ~idxs)
      done;
      output
    | LT.Array ->
      (* TODO: need to handle array... *)
      input
    | _ -> input
  ;;

  let analyze_instr ?(widen = false) penv fenv (instr : instr) (input : t) : t =
    let prog = penv.penv_prog in
    let func, blk = func_of_instr instr, block_of_instr instr in
    let trace =
      if !dfa_path_sensitive then Some (PG.mk_trace_simple instr) else None
    in
    match instr_opcode instr with
    | LO.BitCast -> input
    | LO.Store when is_llvalue_contain_pointer (operand instr 0) ->
      let src, dst = src_of_instr_store instr, dst_of_instr_store instr in
      if LL.is_null dst
      then input
      else (
        (* let _ = hprint "handling store: " pr_instr instr in *)
        (* let _ = hprint "  graph size: " pr_graph_size input in *)
        let sexp = get_bitcast_alias (expr_of_llvalue src) in
        let dexp = get_bitcast_alias (expr_of_llvalue dst) in
        let output = PG.copy input in
        let _ = remove_deref_of_vertex output dexp in
        let _ =
          if not (is_init_func func)
          then (
            (* let dtyp = type_of_expr dexp in *)
            let dst_aliases_precs =
              find_all_aliases_get_precisions
                ~filter:(fun e ->
                  match e with
                  | Var _ -> true
                  (* check_equiv_type dtyp (type_of_expr e) *)
                  | Deref _ -> true
                  | ElemPtr _ -> true
                  (* check_equiv_type dtyp (type_of_expr e) *)
                  | _ -> false)
                prog input dexp May in
            (* (if !dfa_path_sensitive then May else Must) in *)
            (* let _ = hprint "finish finding all aliases of " pr_expr dexp in *)
            (* OPTIMIZATION: add found alias path to pgraph *)
            (* let _ = List.iter ~f:(fun (v, path, prec) ->
             *   insert_alias output prec dexp v) dst_aliases_precs in *)
            let dst_and_aliases =
              dexp :: fst3 (List.unzip3 dst_aliases_precs) in
            let _ =
              List.iter
                ~f:(fun v ->
                  match v with
                  | Var v ->
                    if is_llvalue_param v
                    then
                      (* let _ = hdebug "save deref param: " pr_value v in *)
                      fenv.fenv_deref_params
                        <- List.insert_dedup fenv.fenv_deref_params
                             (mk_param v) ~equal:equal_param
                    else if is_llvalue_global v
                    then (
                      (* let _ = hdebug "save deref global: " pr_value v in *)
                      let ndglobals =
                        List.insert_dedup fenv.fenv_deref_globals (mk_global v)
                          ~equal:equal_global in
                      fenv.fenv_deref_globals <- ndglobals)
                  | _ -> ())
                dst_and_aliases in
            List.iter
              ~f:(fun (v, path, prec) ->
                match prec with
                | May ->
                  if !dfa_path_sensitive
                  then
                    remove_deref_of_vertex ~trace_skip:path.path_trace_all
                      output v
                  else
                    remove_deref_of_vertex ~trace_skip:path.path_trace_all
                      output v
                (* () *)
                | Must -> remove_deref_of_vertex output v)
              dst_aliases_precs) in
        (* let _ = print "finish removing deref!" in *)
        let dderef = deref_of_expr dexp in
        let _ = insert_deref output ~root:dexp ~deref:dderef in
        let _ = insert_alias output Must ~trace sexp dderef in
        output)
    | LO.Load when is_llvalue_contain_pointer (operand instr 0) ->
      let src, dst = src_of_instr_load instr, dst_of_instr_load instr in
      if LL.is_null src
      then input
      else (
        let sexp = get_bitcast_alias (expr_of_llvalue src) in
        let dexp = get_bitcast_alias (expr_of_llvalue dst) in
        let sderef = deref_of_expr sexp in
        let output = PG.copy input in
        let _ = insert_deref output ~root:sexp ~deref:sderef in
        let _ = insert_alias output Must ~trace sderef dexp in
        output)
    | LO.GetElementPtr | LO.ExtractValue ->
      let src = src_of_instr_gep_extract_value instr in
      if is_llvalue_contain_pointer src
      then (
        let rtyp = type_of_llvalue src in
        let dst = dst_of_instr_gep_extract_value instr in
        let sexp = get_bitcast_alias (expr_of_llvalue src) in
        let dexp = expr_of_llvalue dst in
        let idxs = indexes_of_instr_gep_extract_value instr in
        let iexps = List.map ~f:expr_of_llvalue idxs in
        let output = PG.copy input in
        let selemptr = mk_expr_elemptr sexp rtyp iexps in
        (* let _ = insert_element_ptr output ~root:sexp ~rtyp ~elemptr:dexp ~idxs:iexps in *)
        let _ =
          insert_element_ptr output ~root:sexp ~rtyp ~elemptr:selemptr
            ~idxs:iexps in
        let _ = insert_alias output Must ~trace dexp selemptr in
        output)
      else input
    | LO.Call | LO.Invoke ->
      let callee = callee_of_instr_func_call instr in
      let output = PG.copy input in
      if is_func_malloc callee
      then
        output
        (* let size = operand instr 0 in
         * match int_of_const size with
         * | Some s when (s = 0) -> output
         * | _ ->
         *   let src = llvalue_of_instr instr in
         *   let sexp, dexp = mk_expr_malloc src, expr_of_llvalue src in
         *   let _ = insert_alias output Must ~trace sexp dexp in
         *   output *)
      else if is_func_free callee || is_func_cpp_delete callee
      then (
        let _ = hdebug "==> CALL TO DELETE: " func_name callee in
        let src = operand instr 0 in
        let sexp = expr_of_llvalue src in
        free_pointer_from_pgraph prog output sexp)
      else if is_func_memcpy callee || is_func_memmove callee
      then (
        (* FIXME: memcopy seems not being handled properly. Need to fix. *)
        let src, dst = operand instr 1, operand instr 0 in
        let sexp = get_bitcast_alias (expr_of_llvalue src) in
        let dexp = get_bitcast_alias (expr_of_llvalue dst) in
        let styp, dtyp = type_of_expr sexp, type_of_expr dexp in
        let _ = hdebug "Src type: " pr_type styp in
        let elem_typ = LL.element_type styp in
        let _ = hdebug "Elem type: " pr_type elem_typ in
        let _ =
          if equal_type styp dtyp && is_type_struct elem_typ
          then (
            let struct_subelem_types = LL.struct_element_types elem_typ in
            let num_subelem_types = Array.length struct_subelem_types in
            let base_idx = mk_expr_int64 Int64.zero in
            for i = 0 to num_subelem_types - 1 do
              let fld_idx = mk_expr_int64 (Int64.of_int i) in
              let idxs = [ base_idx; fld_idx ] in
              let selem = mk_expr_elemptr sexp styp idxs in
              let delem = mk_expr_elemptr dexp dtyp idxs in
              let _ =
                insert_element_ptr output ~root:sexp ~elemptr:selem ~rtyp:styp
                  ~idxs in
              let _ =
                insert_element_ptr output ~root:dexp ~elemptr:delem ~rtyp:dtyp
                  ~idxs in
              insert_alias output Must ~trace selem delem
            done)
          else (
            let sderef = deref_of_expr sexp in
            let dderef = deref_of_expr dexp in
            let _ = insert_deref output ~root:sexp ~deref:sderef in
            let _ = insert_deref output ~root:dexp ~deref:dderef in
            insert_alias output Must ~trace sderef dderef) in
        (* let edges = [] in
         * let edges = create_memcpy_edges output instr sexp dexp in
         * let _ = List.iter ~f:(PG.add_edge_e output) edges in *)
        (* let _ = hdebug "   handle Memcpy: " pr_expr sexp in
         * let _ = hdebug "   New Edges: " (pr_items PG.pr_edge) edges in *)
        output)
      else if is_func_dynamic_cast callee
      then (
        let src, dst = operand instr 0, llvalue_of_instr instr in
        let sexp = get_bitcast_alias (expr_of_llvalue src) in
        let dexp = expr_of_llvalue dst in
        let _ = insert_alias output Must ~trace sexp dexp in
        output)
      else if is_func_pointer callee
      then (
        (* update new functions pointer list, and pass it back to DFA *)
        let _ = hdebug "pointer: callee: " func_name callee in
        let ptr = llvalue_of_func callee in
        let _ = hdebug "Find function pointer in graph: " pr_pgraph output in
        let funcs =
          match get_bitcast_alias (expr_of_llvalue ptr) with
          | Var vptr ->
            (match LL.classify_value vptr with
            | LV.Function -> [ mk_func vptr ]
            | _ -> find_funcs_of_pointer prog output vptr)
          | _ -> [] in
        let _ =
          debug
            ("- Function pointers of " ^ pr_value ptr ^ ": " ^ func_names funcs)
        in
        output)
      else output
    | LO.PHI when is_llvalue_contain_pointer (llvalue_of_instr instr) ->
      let src_origin = src_and_origin_of_instr_phi instr in
      let src_blks = snd (List.unzip src_origin) in
      let dst = dst_of_instr_phi instr in
      let dexp = expr_of_llvalue dst in
      let output = PG.copy input in
      (* get alias from the sources of this PHI instr *)
      let _ =
        List.iter
          ~f:(fun (src_opr, src_blk) ->
            let sexp = get_bitcast_alias (expr_of_llvalue src_opr) in
            if (not (is_expr_null sexp)) || not (is_expr_undef sexp)
            then (
              let trace =
                match !dfa_path_sensitive with
                | false -> None
                | true ->
                  let skip_blocks =
                    List.filter
                      ~f:(fun b -> not (equal_block b src_blk))
                      src_blks in
                  Some (PG.mk_trace_phi skip_blocks src_blk instr) in
              insert_alias output May ~trace sexp dexp))
          src_origin in
      output
    | LO.Ret when num_operands instr = 1 ->
      let src = src_of_instr_return instr in
      let sexp = get_bitcast_alias (expr_of_llvalue src) in
      let output = PG.copy input in
      if is_llvalue_contain_pointer src
      then (
        let dexp = mk_expr_func_result func in
        let _ = insert_alias output Must ~trace sexp dexp in
        output)
      else output
    | _ -> input
  ;;

  (*******************************************************************
   ** Pre and post analysis
   *******************************************************************)

  let pre_analyze_func penv fenv = ()

  (*******************************************************************
   ** Checking assertions
   *******************************************************************)

  (** Count the number of assertions in a program *)

  let count_assertions (prog : program) : int =
    let assertions =
      List.fold_left
        ~f:(fun acc func -> acc @ AS.find_alias_assertions func)
        ~init:[] prog.prog_user_funcs in
    List.length assertions
  ;;

  let check_may_alias (fenv : func_env) (instr : instr) v1 v2 : bool =
    match get_instr_output fenv instr with
    | None -> false
    | Some data ->
      let u1, u2 = expr_of_llvalue v1, expr_of_llvalue v2 in
      is_may_alias_exp fenv.fenv_prog data u1 u2
  ;;

  let check_must_alias (fenv : func_env) (instr : instr) v1 v2 : bool =
    let _ =
      debug
        ("Checking MustAlias in env(" ^ fenv.fenv_id ^ ")" ^ " of function: "
       ^ func_name fenv.fenv_func) in
    match get_instr_output fenv instr with
    | None -> false
    | Some data ->
      let u1, u2 = expr_of_llvalue v1, expr_of_llvalue v2 in
      is_must_alias_exp fenv.fenv_prog data u1 u2
  ;;

  let check_no_alias (fenv : func_env) (instr : instr) v1 v2 : bool =
    match get_instr_output fenv instr with
    | None -> false
    | Some data ->
      let u1, u2 = expr_of_llvalue v1, expr_of_llvalue v2 in
      is_no_alias_exp fenv.fenv_prog data u1 u2
  ;;

  let check_assertion (fenvs : func_env list) (ast : AS.assertion)
      : bool option
    =
    let instr = ast.AS.ast_instr in
    match ast.AS.ast_type with
    | AS.Assert ->
      (match ast.AS.ast_predicate with
      | AS.NoAlias (v1, v2) ->
        let res =
          List.for_all ~f:(fun fe -> check_no_alias fe instr v1 v2) fenvs in
        Some res
      | AS.MayAlias (v1, v2) ->
        let res =
          List.exists
            ~f:(fun fe ->
              if !dfa_pointer_conservative
              then check_may_alias fe instr v1 v2
              else true)
            fenvs in
        Some res
      | AS.MustAlias (v1, v2) ->
        let res =
          List.for_all ~f:(fun fe -> check_must_alias fe instr v1 v2) fenvs
        in
        Some res
      | _ -> None)
    | AS.Refute ->
      (match ast.AS.ast_predicate with
      | AS.NoAlias (v1, v2) ->
        let res =
          List.exists
            ~f:(fun fe ->
              if !dfa_pointer_conservative
              then check_may_alias fe instr v1 v2
              else true)
            fenvs in
        Some res
      | AS.MayAlias (v1, v2) ->
        let res =
          List.for_all
            ~f:(fun fe ->
              if !dfa_pointer_conservative
              then check_no_alias fe instr v1 v2
              else true)
            fenvs in
        Some res
      | AS.MustAlias (v1, v2) ->
        let res =
          List.for_all
            ~f:(fun fe ->
              check_no_alias fe instr v1 v2 || check_may_alias fe instr v1 v2)
            fenvs in
        Some res
      | _ -> None)
  ;;

  let check_assertions (penv : prog_env) (func : func) : int =
    let assertions = AS.find_alias_assertions func in
    let fenvs =
      match Hashtbl.find penv.penv_func_envs func with
      | None -> []
      | Some fenvs -> fenvs in
    let num_checked_assertions = ref 0 in
    let _ =
      List.iter
        ~f:(fun ast ->
          match check_assertion fenvs ast with
          | Some res ->
            let _ = incr num_checked_assertions in
            let _ =
              if res then incr num_valid_asserts else incr num_invalid_asserts
            in
            print_endline (AS.pr_assertion_status func ast res)
          | None -> ())
        assertions in
    !num_checked_assertions
  ;;
end

(*******************************************************************
 ** Main analysis module
 *******************************************************************)

module Analysis = struct
  include PointerTransfer
  include DF.ForwardDataFlow (PointerTransfer)
end
