(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
open Llprinter.PrinterPrimitives
module LL = Llvm
module LT = LL.TypeKind
module LV = LL.ValueKind
module LO = LL.Opcode
module SP = Set.Poly

module AST = struct
  (*******************************************************************
   ** Core data structures of a program
   *******************************************************************)

  (** [Value] is an important data structure in LLVM IR.
    It's a base data structure for instructions, functions,
    global variables, constant, etc. *)
  type value = LL.llvalue

  (** Data type of [value]. *)
  type datatype = LL.lltype

  (** A [use] represents a user or a usee of a value *)
  type use = LL.lluse

  (** Comparison predicate for integer types *)
  type icompare = LL.Icmp.t

  (** Comparison predicate for floating-point types *)
  type fcompare = LL.Fcmp.t

  (** Global variable *)
  type global = Global of value

  (** Constant value *)
  type const = Constant of value

  (** Instruction of programs *)
  type instr = Instr of value

  (** Basic block of functions *)
  type block = LL.llbasicblock

  (** Formal parameter of functions *)
  type param = Param of value

  (** Function in programs *)
  type func = Func of value

  (** A [callable] is a function or a function pointer *)
  type callable =
    | ClFunc of func
    | ClFPtr of value

  (** Bitcode module of a program *)
  type bitcode_module = LL.llmodule

  (** Expression *)
  type expr =
    | Undef of datatype
    | Int64 of int64
    | Float of float
    | String of string
    | Var of value
    | OldE of expr
    | Deref of expr
    | ElemPtr of (expr * datatype * expr list)
    | Malloc of expr (* allocation *)
    | FuncRes of func (* returned value of a function *)
    | Exn of expr

  (* Some convenient types *)

  type values = value list
  type datatypes = datatype list
  type uses = use list
  type params = param list
  type instrs = instr list
  type globals = global list
  type consts = const list
  type exprs = expr list
  type blocks = block list
  type funcs = func list
  type callables = callable list

  (*******************************************************************
   ** Auxiliary data structures
   *******************************************************************)

  type predicate =
    | PBool of bool
    | PIcmp of (icompare * value * value)
    | PFcmp of (fcompare * value * value)
    | PNeg of predicate
    | PConj of predicate list
    | PDisj of predicate list

  (* preceding block of a block *)
  type prec_block =
    { pblk_block : block;
      pblk_pathcond : predicate
    }

  (* succeeding block of a block *)
  type succ_block =
    { sblk_block : block;
      sblk_pathcond : predicate
    }

  type prec_blocks = prec_block list
  type succ_blocks = succ_block list

  type loop =
    { loop_head : block;
      loop_body : block list;
      loop_exit : block list;
      loop_exit_reachables : block list;
      loop_inners : loop list;
      loop_outers : loop option
    }

  type loops = loop list

  (*******************************************************************
   ** Modules of data structures used for Hashtbl
   *******************************************************************)

  module TypeKey = struct
    type t = datatype

    let to_string _ = "(datatype)"
    let hash = Hashtbl.hash
    let sexp_of_t _ = Sexp.of_string "(datatype)"
    let compare = Poly.compare
  end

  module ValueKey = struct
    type t = value

    let to_string _ = "(value)"
    let hash = Hashtbl.hash
    let sexp_of_t _ = Sexp.of_string "(value)"
    let compare = Poly.compare
  end

  module InstrKey = struct
    type t = instr

    let to_string _ = "(instr)"
    let hash = Hashtbl.hash
    let sexp_of_t _ = Sexp.of_string "(instr)"
    let compare = Poly.compare
  end

  module GlobalKey = struct
    type t = global

    let to_string _ = "(global)"
    let hash = Hashtbl.hash
    let sexp_of_t _ = Sexp.of_string "(global)"
    let compare = Poly.compare
  end

  module FuncKey = struct
    type t = func

    let to_string _ = "(func)"
    let hash = Hashtbl.hash
    let sexp_of_t _ = Sexp.of_string "(func)"
    let compare = Poly.compare
  end

  module CallableKey = struct
    type t = callable

    let to_string _ = "(callable)"
    let hash = Hashtbl.hash
    let sexp_of_t _ = Sexp.of_string "(callable)"
    let compare = Poly.compare
  end

  module UseKey = struct
    type t = use

    let to_string _ = "(use)"
    let hash = Hashtbl.hash
    let sexp_of_t _ = Sexp.of_string "(use)"
    let compare = Poly.compare
  end

  module ExprKey = struct
    type t = expr

    let to_string _ = "(expr)"
    let hash = Hashtbl.hash
    let sexp_of_t _ = Sexp.of_string "(expr)"
    let compare = Poly.compare
  end

  module BlockKey = struct
    type t = block

    let to_string _ = "(block)"
    let hash = Hashtbl.hash
    let sexp_of_t _ = Sexp.of_string "(block)"
    let compare = Poly.compare
  end

  (*******************************************************************
   ** more modules for LLIR
   *******************************************************************)

  module CallGraph = struct
    module Vertex = struct
      type t = func

      let compare = Poly.compare

      let equal f1 f2 =
        match f1, f2 with
        | Func v1, Func v2 -> v1 == v2
      ;;

      let hash = Hashtbl.hash
    end

    include Graph.Imperative.Digraph.Concrete (Vertex)
  end

  module BlockGraph = struct
    type label = NextBlock

    module Vertex = struct
      type t = block

      let compare = Poly.compare
      let equal b1 b2 = b1 == b2
      let hash = Hashtbl.hash
    end

    module Edge = struct
      type t = label

      let compare = Poly.compare
      let equal l1 l2 = l1 == l2
      let hash = Hashtbl.hash
      let default = NextBlock
    end

    module G = Graph.Imperative.Digraph.Concrete (Vertex)
    include G

    module Weight = struct
      type edge = G.E.t
      type t = int

      let weight _ = 1
      let compare = Int.compare
      let add = Int.( + )
      let zero = 0
    end

    module Dijkstra = Graph.Path.Dijkstra (G) (Weight)
  end

  module InstrGraph = struct
    type rvertex = value

    type label =
      | NextInstr
      | Callee

    module Vertex = struct
      type t = rvertex

      let compare = Poly.compare
      let equal v1 v2 = v1 == v2
      let hash = Hashtbl.hash
    end

    module Edge = struct
      type t = label

      let compare = Poly.compare
      let equal l1 l2 = l1 == l2
      let hash = Hashtbl.hash
      let default = NextInstr
    end

    module G = Graph.Imperative.Digraph.ConcreteLabeled (Vertex) (Edge)
    include G

    module Weight = struct
      type edge = G.E.t
      type t = int

      let weight _ = 1
      let compare = Int.compare
      let add = Int.( + )
      let zero = 0
    end

    module Dijkstra = Graph.Path.Dijkstra (G) (Weight)

    (* module Pathcheck = Graph.Path.Check(G) *)
  end

  module CG = CallGraph
  module BG = BlockGraph
  module IG = InstrGraph

  (*******************************************************************
   ** Program data structure
   *******************************************************************)

  type program_block_data =
    { pbd_preceding_blocks : (block, prec_blocks) Hashtbl.t;
      pbd_succeeding_blocks : (block, succ_blocks) Hashtbl.t;
      pbd_incoming_pathcond : (block, predicate list) Hashtbl.t;
      pbd_reachable_blocks : (block, block list) Hashtbl.t
    }

  type program_loop_data =
    { pld_loop_updated_instr : (instr, bool) Hashtbl.t;
      pld_loop_head_instr : (instr, bool) Hashtbl.t;
      pld_innermost_loop_containing_block : (block, loop option) Hashtbl.t;
      pld_innermost_loop_containing_value : (value, loop option) Hashtbl.t
    }

  type program_func_data =
    { pfd_return_instr : (func, instr) Hashtbl.t;
      pfd_callers : (func, funcs) Hashtbl.t;
      pfd_callees : (func, callables) Hashtbl.t;
      pfd_reachable_funcs : (func, funcs) Hashtbl.t;
      pfd_loops : (func, loops) Hashtbl.t;
      pfd_used_globals : (func, globals) Hashtbl.t;
      pfd_func_call_graph : CallGraph.t;
      pfd_block_graph : (func, BlockGraph.t) Hashtbl.t;
      pfd_funcs_of_pointer : (value, funcs) Hashtbl.t;
      mutable pfd_funcs_of_type : (datatype, funcs) Hashtbl.t
    }

  type program_meta_data =
    { pmd_bitcode_filename : string;
      pmd_source_filename : string;
      pmd_llvalue_original_name : (string, string) Hashtbl.t;
      pmd_module_id : string;
      pmd_data_layout : string;
      pmd_target_platform : string
    }

  type program =
    { prog_globals : global list;
      prog_struct_types : datatype list; (* data structures in function *)
      prog_all_funcs : func list; (* All functions of a program *)
      prog_lib_no_source_funcs : func list; (* Libraries, won't be analyzed *)
      prog_lib_has_source_funcs : func list; (* Libraries, may be analyzed *)
      prog_discover_funcs : func list; (* Built-in functions of Discover *)
      prog_init_funcs : func list; (* Initialization Functions *)
      prog_user_funcs : func list; (* User code which will be analyzed *)
      prog_entry_funcs : func list; (* Entry point of program *)
      (* program data *)
      prog_block_data : program_block_data;
      prog_loop_data : program_loop_data;
      prog_func_data : program_func_data;
      prog_meta_data : program_meta_data;
      prog_module_data : bitcode_module
    }
end

include AST

(*-----------------------------------------
 * datatype
 *-----------------------------------------*)

module Type = struct
  let equal_type (t1 : datatype) (t2 : datatype) : bool = t1 == t2
  let pr_type (t : datatype) : string = String.strip (Llvm.string_of_lltype t)

  let is_type_void (typ : datatype) : bool =
    match LL.classify_type typ with
    | LT.Void -> true
    | _ -> false
  ;;

  let is_type_integer (typ : datatype) : bool =
    match LL.classify_type typ with
    | LT.Integer -> true
    | _ -> false
  ;;

  let is_type_array (typ : datatype) : bool =
    match LL.classify_type typ with
    | LT.Array -> true
    | _ -> false
  ;;

  let is_type_string (typ : datatype) : bool =
    match LL.classify_type typ with
    | LT.Array ->
      let elemtyp = LL.element_type typ in
      (match LL.classify_type elemtyp with
      | LT.Integer -> LL.integer_bitwidth elemtyp = 8
      | _ -> false)
    | _ -> false
  ;;

  let is_type_struct (typ : datatype) : bool =
    match LL.classify_type typ with
    | LT.Struct -> true
    | _ -> false
  ;;

  let is_type_pointer (typ : datatype) : bool =
    match LL.classify_type typ with
    | LT.Pointer -> true
    | _ -> false
  ;;

  let rec is_type_contain_pointer (typ : datatype) : bool =
    match LL.classify_type typ with
    | LT.Pointer -> true
    | LT.Struct ->
      let elem_types = Array.to_list (LL.struct_element_types typ) in
      List.exists ~f:is_type_contain_pointer elem_types
    | _ -> false
  ;;

  let is_type_func (typ : datatype) : bool =
    match LL.classify_type typ with
    | LT.Function -> true
    | _ -> false
  ;;

  let is_type_pointer_to_func (typ : datatype) : bool =
    match LL.classify_type typ with
    | LT.Pointer -> is_type_func (LL.element_type typ)
    | _ -> false
  ;;

  (** Memory size in bytes of an element of type `typ' *)

  let memsize_of_type (typ : datatype) : int64 =
    match LL.classify_type typ with
    | LT.Integer -> Int64.of_int (LL.integer_bitwidth typ / 8)
    | _ -> herror "memsize_of_size: need to implement: " pr_type typ
  ;;

  let rec get_elemptr_typ (typ : datatype) (idxs : expr list) : datatype =
    match idxs with
    | [] -> typ
    | idx :: nidxs ->
      let ntyp =
        match LL.classify_type typ with
        | LT.Struct ->
          let fld_idx =
            match idx with
            | Int64 i -> Int64.to_int_exn i
            | _ -> error "get_elemptr_typ: can't get struct field idx" in
          Array.get (LL.subtypes typ) fld_idx
        | LT.Array -> LL.element_type typ
        | LT.Pointer -> LL.element_type typ
        | _ -> herror "get_elemptr_typ: need to handle type: " pr_type typ
      in
      get_elemptr_typ ntyp nidxs
  ;;

  let rec check_equiv_type (t1 : datatype) (t2 : datatype) : bool =
    if equal_type t1 t2
    then true
    else (
      match LL.classify_type t1, LL.classify_type t2 with
      | LT.Struct, LT.Struct ->
        let etypes1 = LL.struct_element_types t1 in
        let etypes2 = LL.struct_element_types t2 in
        if Array.length etypes1 = Array.length etypes2
        then
          (* let ts1 = Array.to_list etypes1 in
           * let ts2 = Array.to_list etypes2 in *)
          Array.for_all2_exn ~f:check_equiv_type etypes1 etypes2
        else false
      | LT.Array, LT.Array ->
        check_equiv_type (LL.element_type t1) (LL.element_type t2)
      | LT.Pointer, LT.Pointer ->
        check_equiv_type (LL.element_type t1) (LL.element_type t2)
      | LT.Vector, LT.Vector ->
        check_equiv_type (LL.element_type t1) (LL.element_type t2)
      | _ -> true)
  ;;
end

include Type

(*-----------------------------------------
 * LLVM Value
 *-----------------------------------------*)

(** Module contains operations over value *)
module Value = struct
  let equal_value (v1 : value) (v2 : value) : bool = v1 == v2

  let pr_value (v : LL.llvalue) : string =
    let vtyp = LL.type_of v in
    let vname =
      if LL.is_undef v
      then "undef"
      else (
        match LL.classify_type vtyp with
        | LT.Integer ->
          (match LL.int64_of_const v with
          | Some a -> Int64.to_string a
          | None -> LL.value_name v)
        | LT.Float ->
          (match LL.float_of_const v with
          | Some a -> pr_float a
          | None -> LL.value_name v)
        | LT.Pointer -> if LL.is_null v then "null" else LL.value_name v
        | LT.Metadata -> LL.string_of_llvalue v
        | _ -> LL.string_of_llvalue v) in
    if String.is_empty vname
    then "<raw-name: " ^ LL.string_of_llvalue v ^ ">"
    else vname
  ;;

  let pr_values (vs : LL.llvalue list) : string = pr_list ~f:pr_value vs

  let pr_value_detail (v : LL.llvalue) : string =
    v |> LL.string_of_llvalue |> String.split_lines |> List.map ~f:String.strip
    |> String.concat ~sep:" "
  ;;

  let value_name (v : value) : string = pr_value v
  let value_names (vs : values) : string = pr_list ~f:pr_value vs
  let pr_values_detail (vs : values) : string = pr_list ~f:pr_value_detail vs
  let equal_value (v1 : value) (v2 : value) : bool = equal_value v1 v2

  let is_llvalue_empty_name (v : value) : bool =
    String.is_empty (LL.value_name v)
  ;;

  let is_llvalue_void (v : value) : bool = is_type_void (Llvm.type_of v)

  let is_llvalue_undef (v : value) : bool =
    match LL.classify_value v with
    | LV.UndefValue -> true
    | _ -> false
  ;;

  let is_llvalue_pointer (v : value) : bool = is_type_pointer (LL.type_of v)

  let is_llvalue_contain_pointer (v : value) : bool =
    is_type_contain_pointer (LL.type_of v)
  ;;

  let is_llvalue_pointer_to_function (v : value) : bool =
    let typ = LL.type_of v in
    is_type_pointer typ && is_type_pointer (LL.element_type typ)
  ;;

  let is_llvalue_param (v : value) : bool =
    match LL.classify_value v with
    | LV.Argument _ -> true
    | _ -> false
  ;;

  let is_llvalue_instr (v : value) : bool =
    match LL.classify_value v with
    | LV.Instruction _ -> true
    | _ -> false
  ;;

  let is_llvalue_instr_phi (v : value) : bool =
    match LL.classify_value v with
    | LV.Instruction LO.PHI -> true
    | _ -> false
  ;;

  let is_llvalue_callable_instr (v : value) : bool =
    match LL.classify_value v with
    | LV.Instruction LO.Call -> true
    | LV.Instruction LO.Invoke -> true
    | _ -> false
  ;;

  let is_llvalue_instr_gep (v : value) : bool =
    match LL.classify_value v with
    | LV.Instruction LO.GetElementPtr -> true
    | _ -> false
  ;;

  let is_local_llvalue (v : value) : bool = is_llvalue_instr v

  let is_llvalue_constant_expr (v : value) : bool =
    match LL.classify_value v with
    | LV.ConstantExpr -> true
    | _ -> false
  ;;

  let is_llvalue_null_constant (v : value) : bool = LL.is_null v

  let is_llvalue_integer_constant (v : value) : bool =
    if LL.is_constant v
    then (
      match LL.classify_type (LL.type_of v) with
      | LT.Integer -> true
      | _ -> false)
    else false
  ;;

  let is_llvalue_argument (v : value) : bool =
    match LL.classify_value v with
    | LV.Argument -> true
    | _ -> false
  ;;

  let is_llvalue_global (v : value) : bool =
    match LL.classify_value v with
    | LV.GlobalVariable -> true
    | _ -> false
  ;;

  let is_llvalue_function (v : value) : bool =
    match LL.classify_value v with
    | LV.Function -> true
    | _ -> false
  ;;

  let type_of_llvalue (v : value) : datatype = LL.type_of v
end

include Value

(*-----------------------------------------
 * global
 *-----------------------------------------*)

module Global = struct
  (*** Comparisons ***)

  let equal_global (g1 : global) (g2 : global) : bool =
    match g1, g2 with
    | Global v1, Global v2 -> equal_value v1 v2
  ;;

  (*** Constructors  ***)

  let mk_global (v : value) : global =
    match LL.classify_value v with
    | LV.GlobalVariable -> Global v
    | _ -> herror "mk_global: not a global variable: " pr_value_detail v
  ;;

  let llvalue_of_global (g : global) : value =
    match g with
    | Global v -> v
  ;;

  let llvalues_of_globals (gs : globals) : values =
    List.map ~f:llvalue_of_global gs
  ;;

  (*** Printings ***)

  let pr_global ?(detailed = false) (g : global) : string =
    match g with
    | Global v -> if detailed then pr_value_detail v else pr_value v
  ;;

  let pr_globals (gs : globals) : string = pr_list ~f:pr_global gs
  let global_name (g : global) : string = pr_global ~detailed:false g
  let global_names (gs : globals) : string = pr_args ~f:global_name gs

  let global_operand (g : global) (idx : int) : value =
    LL.operand (llvalue_of_global g) idx
  ;;

  let type_of_global (g : global) : datatype = LL.type_of (llvalue_of_global g)
end

include Global

(*-----------------------------------------
 * Constants
 *-----------------------------------------*)

module Const = struct
  (*** Comparisons ***)

  let equal_const (c1 : const) (c2 : const) : bool =
    match c1, c2 with
    | Constant v1, Constant v2 -> equal_value v1 v2
  ;;

  (*** Constructors ***)

  let mk_const (v : value) : const =
    match LL.classify_value v with
    | LV.ConstantExpr -> Constant v
    | _ -> herror "mk_const: not a constant: " pr_value_detail v
  ;;

  (*** Conversions ***)

  let llvalue_of_const (c : const) : value =
    match c with
    | Constant v -> v
  ;;

end

include Const

(*-----------------------------------------
 * instruction
 *-----------------------------------------*)

module Instr = struct
  let equal_instr (i1 : instr) (i2 : instr) : bool =
    match i1, i2 with
    | Instr v1, Instr v2 -> equal_value v1 v2
  ;;

  let mk_instr (v : value) : instr =
    match LL.classify_value v with
    | LV.Instruction _ -> Instr v
    | _ -> herror "mk_instr: not an instruction: " pr_value_detail v
  ;;

  let llvalue_of_instr (i : instr) : value =
    match i with
    | Instr v -> v
  ;;

  let pr_instr (i : instr) : string =
    match i with
    | Instr v -> pr_value_detail v
  ;;

  let instr_opcode (i : instr) : LO.t = LL.instr_opcode (llvalue_of_instr i)

  let operand (i : instr) (idx : int) : value =
    LL.operand (llvalue_of_instr i) idx
  ;;

  let num_operands (i : instr) : int = LL.num_operands (llvalue_of_instr i)

  let set_operand (i : instr) (idx : int) (opr : value) : unit =
    LL.set_operand (llvalue_of_instr i) idx opr
  ;;

  let operands (i : instr) : value list =
    let oprs = ref [] in
    for idx = 0 to num_operands i - 1 do
      oprs := !oprs @ [ operand i idx ]
    done;
    !oprs
  ;;

  let get_branch (i : instr) = LL.get_branch (llvalue_of_instr i)
  let block_of_instr (i : instr) : block = LL.instr_parent (llvalue_of_instr i)
  let type_of_instr (i : instr) : datatype = LL.type_of (llvalue_of_instr i)

  let is_instr_load (i : instr) : bool =
    match instr_opcode i with
    | LO.Load -> true
    | _ -> false
  ;;

  let is_instr_store (i : instr) : bool =
    match instr_opcode i with
    | LO.Store -> true
    | _ -> false
  ;;

  let is_instr_gep (i : instr) : bool =
    match instr_opcode i with
    | LO.GetElementPtr -> true
    | _ -> false
  ;;

  let is_instr_extractvalue (i : instr) : bool =
    match instr_opcode i with
    | LO.ExtractValue -> true
    | _ -> false
  ;;

  let is_instr_call (i : instr) : bool =
    match instr_opcode i with
    | LO.Call -> true
    | _ -> false
  ;;

  let is_instr_invoke (i : instr) : bool =
    match instr_opcode i with
    | LO.Invoke -> true
    | _ -> false
  ;;

  let is_instr_call_invoke (i : instr) : bool =
    match instr_opcode i with
    | LO.Call | LO.CallBr | LO.Invoke -> true
    | _ -> false
  ;;

  let is_instr_return (i : instr) : bool =
    match instr_opcode i with
    | LO.Ret -> true
    | _ -> false
  ;;

  let is_instr_bitcast (i : instr) : bool =
    match instr_opcode i with
    | LO.BitCast -> true
    | _ -> false
  ;;

  let is_instr_br_or_switch (i : instr) : bool =
    match instr_opcode i with
    | LO.Br | LO.IndirectBr | LO.Switch -> true
    | _ -> false
  ;;

  let is_instr_unreachable (i : instr) : bool =
    match instr_opcode i with
    | LO.Unreachable -> true
    | _ -> false
  ;;

  let delete_instruction (i : instr) : unit =
    LL.delete_instruction (llvalue_of_instr i)
  ;;
end

include Instr

(*-----------------------------------------
 * function and parameters
 *-----------------------------------------*)

module Func = struct
  (*** Comparisons ***)

  let equal_func (f1 : func) (f2 : func) : bool =
    match f1, f2 with
    | Func v1, Func v2 -> equal_value v1 v2
  ;;

  let equal_param (p1 : param) (p2 : param) : bool =
    match p1, p2 with
    | Param v1, Param v2 -> equal_value v1 v2
  ;;

  (*** Constructors ***)

  let mk_func (v : value) : func =
    match LL.classify_value v with
    | LV.Function -> Func v
    | LV.Instruction _ -> Func v
    | LV.Argument _ -> Func v
    | _ -> Func v
  ;;

  let llvalue_of_func (f : func) : value =
    match f with
    | Func v -> v
  ;;

  let mk_param (v : value) : param =
    match LL.classify_value v with
    | LV.Argument -> Param v
    | _ -> herror "mk_param: not a formal parameter: " pr_value_detail v
  ;;

  let llvalue_of_param (p : param) : value =
    match p with
    | Param v -> v
  ;;

  let llvalues_of_params (ps : params) : values =
    List.map ~f:llvalue_of_param ps
  ;;

  let pr_param ?(detailed = false) (p : param) : string =
    match p with
    | Param v -> if detailed then pr_value_detail v else pr_value v
  ;;

  let pr_params (ps : params) : string = pr_args ~f:pr_param ps

  let pr_typed_param (p : param) : string =
    match p with
    | Param v -> pr_type (LL.type_of v) ^ " " ^ pr_value v
  ;;

  let func_name (f : func) : string =
    let v = llvalue_of_func f in
    match LL.classify_value v with
    | LV.Function -> LL.value_name v
    | LV.Instruction _ -> LL.value_name v
    | LV.Argument _ -> LL.value_name v
    | _ -> LL.value_name v
  ;;

  let param_name (p : param) : string = pr_value (llvalue_of_param p)
  let param_names (ps : params) : string = pr_args ~f:param_name ps

  let func_name_and_params (f : func) : string =
    let v = llvalue_of_func f in
    match LL.classify_value v with
    | LV.Function ->
      let params = Array.to_list (LL.params v) in
      LL.value_name v ^ "(" ^ pr_args ~f:pr_value_detail params ^ ")"
    | _ -> herror "func_name_and_params: not a function: " pr_value_detail v
  ;;

  let func_names (fs : func list) : string = pr_list ~f:func_name fs

  let func_params (f : func) : param list =
    let v = llvalue_of_func f in
    match LL.classify_value v with
    | LV.Function ->
      let vparams = Array.to_list (LL.params v) in
      List.map ~f:mk_param vparams
    | _ -> herror "func_params: not a function: " pr_value_detail v
  ;;

  let func_type (f : func) : datatype =
    LL.return_type (LL.type_of (llvalue_of_func f))
  ;;

  let func_param_types (f : func) : datatype list =
    Array.to_list (LL.param_types (func_type f))
  ;;

  let func_return_type (f : func) : datatype = LL.return_type (func_type f)

  let blocks_of_func (f : func) : blocks =
    let v = llvalue_of_func f in
    LL.fold_left_blocks (fun acc blk -> acc @ [ blk ]) [] v
  ;;

  let is_func_free (f : func) : bool = String.equal (func_name f) "free"
  let is_func_malloc (f : func) : bool = String.equal (func_name f) "malloc"
  let is_func_realloc (f : func) : bool = String.equal (func_name f) "realloc"
  let is_func_nondet (f : func) : bool = String.equal (func_name f) "__nondet"

  let is_func_handling_exception (f : func) : bool =
    let fname = func_name f in
    String.equal fname "__cxa_allocate_exception"
    || String.equal fname "__cxa_throw"
    || String.equal fname "__cxa_begin_catch"
    || String.equal fname "__cxa_end_catch"
    || String.equal fname "llvm.eh.typeid.for"
  ;;

  let is_func_memcpy (f : func) : bool =
    String.is_prefix (func_name f) ~prefix:"llvm.memcpy"
  ;;

  let is_func_memmove (f : func) : bool =
    String.is_prefix (func_name f) ~prefix:"llvm.memmove"
  ;;

  let is_func_scanf (f : func) : bool =
    let fname = func_name f in
    String.equal fname "__isoc99_scanf"
  ;;

  let is_func_dynamic_cast (f : func) : bool =
    String.equal (func_name f) "__dynamic_cast"
  ;;

  let is_func_clang_call_terminate (f : func) : bool =
    String.equal (func_name f) "__clang_call_terminate"
  ;;

  let is_func_cpp_new (f : func) : bool = String.equal (func_name f) "_Znwm"

  let is_func_cpp_delete (f : func) : bool =
    String.equal (func_name f) "_ZdlPv"
  ;;

  let is_func_main (f : func) : bool = String.equal (func_name f) "main"

  let is_func_llvm_debug (f : func) : bool =
    let fname = func_name f in
    String.equal fname "llvm.dbg.declare"
    || String.equal fname "llvm.dbg.value"
  ;;

  let is_func_llvm_debug_declare (f : func) : bool =
    String.equal (func_name f) "llvm.dbg.declare"
  ;;

  let is_func_llvm_debug_value (f : func) : bool =
    String.equal (func_name f) "llvm.dbg.value"
  ;;

  let is_lib_no_source_func (f : func) : bool =
    List.is_empty (blocks_of_func f)
    || is_func_free f || is_func_malloc f || is_func_realloc f
    || is_func_nondet f
  ;;

  let is_lib_has_source_func (f : func) : bool =
    let v = llvalue_of_func f in
    match LL.classify_value v with
    | LV.Function -> LL.visibility v == LL.Visibility.Hidden
    | _ -> false
  ;;

  let is_discover_assertion_func (f : func) : bool =
    let fname = func_name f in
    String.is_substring ~substring:__assert fname
    || String.is_substring ~substring:__refute fname
  ;;

  let is_init_func (f : func) : bool =
    let fname = func_name f in
    String.is_prefix ~prefix:__init fname
  ;;

  let is_user_func (f : func) : bool =
    let v = llvalue_of_func f in
    match LL.classify_value v with
    | LV.Function ->
      not
        (is_init_func f || is_lib_no_source_func f || is_lib_has_source_func f
        || is_discover_assertion_func f)
    | _ -> false
  ;;

  let is_func_throw_exception (f : func) : bool =
    let fname = func_name f in
    String.equal fname "__cxa_throw"
  ;;

  let is_func_begin_catch_exception (f : func) : bool =
    let fname = func_name f in
    String.equal fname "__cxa_begin_catch"
  ;;

  let is_func_eh_typeid_for (f : func) : bool =
    let fname = func_name f in
    String.equal fname "llvm.eh.typeid.for"
  ;;

  let is_func_pointer (f : func) : bool =
    let v = llvalue_of_func f in
    match LL.classify_value v with
    | LV.Instruction _ -> true
    | _ -> false
  ;;

  let is_func_real_func (f : func) : bool =
    let v = llvalue_of_func f in
    match LL.classify_value v with
    | LV.Function _ -> true
    | _ -> false
  ;;

  let func_of_block (blk : block) : func = mk_func (LL.block_parent blk)

  let func_of_instr (i : instr) : func =
    let vi = llvalue_of_instr i in
    func_of_block (LL.instr_parent vi)
  ;;

  let type_of_func (f : func) : datatype = LL.type_of (llvalue_of_func f)
end

include Func

(*-----------------------------------------
 * Callable: function or function pointer
 *-----------------------------------------*)

module Callable = struct
  let callable_name (c : callable) : string =
    match c with
    | ClFunc f -> Func.func_name f
    | ClFPtr fp -> value_name fp
  ;;

  let mk_callable_func (f : func) : callable = ClFunc f
  let mk_callable_func_pointer (fp : value) : callable = ClFPtr fp
end

include Callable

(*-----------------------------------------
 * Block
 *-----------------------------------------*)

module Block = struct
  let equal_block (blk1 : block) (blk2 : block) : bool = blk1 == blk2

  let mk_prec_block (blk : block) (pcond : predicate) : prec_block =
    { pblk_block = blk; pblk_pathcond = pcond }
  ;;

  let mk_succ_block (blk : block) (pcond : predicate) : succ_block =
    { sblk_block = blk; sblk_pathcond = pcond }
  ;;

  let rec pr_predicate (p : predicate) : string =
    match p with
    | PBool b -> pr_bool b
    | PIcmp (cmp, lhs, rhs) -> pr_value lhs ^ pr_icmp cmp ^ pr_value rhs
    | PFcmp (cmp, lhs, rhs) -> pr_value lhs ^ pr_fcmp cmp ^ pr_value rhs
    | PNeg p -> "!" ^ pr_predicate p
    | PConj ps -> pr_list_plain ~sep:" & " ~f:pr_predicate ps
    | PDisj ps -> pr_list_plain ~sep:" | " ~f:pr_predicate ps
  ;;

  let block_name (blk : block) : string = LL.value_name (LL.value_of_block blk)
  let block_names (blks : block list) : string = pr_list ~f:block_name blks

  let pr_prec_block (pblk : prec_block) : string =
    let blk, p = pblk.pblk_block, pblk.pblk_pathcond in
    "Preceding BlockKey: { " ^ block_name blk ^ "; " ^ pr_predicate p ^ "}"
  ;;

  let pr_prec_blocks (pblks : prec_block list) : string =
    pr_items ~f:pr_prec_block pblks
  ;;

  let pr_succ_block (sblk : succ_block) : string =
    let blk, p = sblk.sblk_block, sblk.sblk_pathcond in
    "Succeeding BlockKey: { " ^ block_name blk ^ "; " ^ pr_predicate p ^ "}"
  ;;

  let equal_block_name (blk1 : block) (blk2 : block) : bool =
    let name1 = block_name blk1 in
    let name2 = block_name blk2 in
    String.equal name1 name2
  ;;
end

include Block

(*-----------------------------------------
 * Loops
 *-----------------------------------------*)

(** Module contains operations over loops *)
module Loop = struct
  let equal_loop (lp1 : loop) (lp2 : loop) : bool =
    equal_block lp1.loop_head lp2.loop_head
  ;;

  let mk_loop ~(head : block) ~(body : block list) ~(exit : block list) =
    { loop_head = head;
      loop_body = body;
      loop_exit = exit;
      loop_exit_reachables = [];
      loop_inners = [];
      loop_outers = None
    }
  ;;
end

include Loop

(*-----------------------------------------
 * Expression
 *-----------------------------------------*)

(** Module contains operations over expressions *)
module Expr = struct
  let mk_expr_undef (typ : datatype) : expr = Undef typ
  let mk_expr_int64 (i : int64) : expr = Int64 i
  let mk_expr_int (i : int) : expr = Int64 (Int64.of_int i)
  let mk_expr_float (f : float) : expr = Float f
  let mk_expr_string (s : string) : expr = String s
  let mk_expr_var (v : value) : expr = Var v
  let mk_expr_deref (v : value) : expr = Deref (mk_expr_var v)

  let mk_expr_elemptr (root : expr) (rtyp : datatype) (idxs : expr list) : expr
    =
    ElemPtr (root, rtyp, idxs)
  ;;

  let mk_expr_malloc (v : value) : expr = Malloc (mk_expr_var v)
  let mk_expr_func_result (f : func) : expr = FuncRes f
  let mk_expr_exn (tinfo : value) : expr = Exn (mk_expr_var tinfo)
  let expr_of_int32 (i : int) = mk_expr_int64 (Int64.of_int i)

  let expr_of_llvalue (v : value) : expr =
    match LL.int64_of_const v, LL.float_of_const v, LL.string_of_const v with
    | None, None, None -> mk_expr_var v
    | Some i, _, _ -> mk_expr_int64 i
    | _, Some f, _ -> mk_expr_float f
    | _, _, Some s -> mk_expr_string s
  ;;

  let expr_of_instr (i : instr) : expr = mk_expr_var (llvalue_of_instr i)
  let expr_of_global (g : global) : expr = mk_expr_var (llvalue_of_global g)
  let deref_of_global (g : global) : expr = mk_expr_deref (llvalue_of_global g)

  let deref_of_expr (e : expr) : expr =
    match e with
    | Var v -> mk_expr_deref v
    | _ -> Deref e
  ;;

  let elemptr_of_global (g : global) (idxs : expr list) =
    let gtyp = LL.type_of (llvalue_of_global g) in
    mk_expr_elemptr (expr_of_global g) gtyp idxs
  ;;

  let rec pr_expr (e : expr) : string =
    match e with
    | Undef t -> "undef"
    | Int64 i -> Int64.to_string i
    | Float f -> pr_float f
    | String s -> "\"" ^ s ^ "\""
    | Var v -> pr_value v
    | OldE e -> pr_expr e ^ "'"
    | Deref e -> "*" ^ pr_expr e
    | ElemPtr (root, rtyp, idxs) ->
      let sidxs = idxs |> List.map ~f:pr_expr |> String.concat ~sep:"," in
      "EP(" ^ pr_expr root ^ "," ^ pr_type rtyp ^ "," ^ sidxs ^ ")"
    | Malloc e -> "Malloc(" ^ pr_expr e ^ ")"
    | FuncRes f -> "res_" ^ pr_value (llvalue_of_func f)
    | Exn e -> "exn_" ^ pr_expr e
  ;;

  let pr_exprs (es : expr list) : string = pr_list ~f:pr_expr es

  let rec equal_expr (e1 : expr) (e2 : expr) : bool =
    match e1, e2 with
    | Undef t1, Undef t2 -> equal_type t1 t2
    | Undef _, _ -> false
    | Int64 i1, Int64 i2 -> Int64.equal i1 i2
    | Int64 _, _ -> false
    | Float f1, Float f2 -> Float.equal f1 f2
    | Float _, _ -> false
    | String s1, String s2 -> String.equal s1 s2
    | String _, _ -> false
    | Var v1, Var v2 -> equal_value v1 v2
    | Var _, _ -> false
    | OldE e1, OldE e2 -> equal_expr e1 e2
    | OldE _, _ -> false
    | Deref e1, Deref e2 -> equal_expr e1 e2
    | Deref _, _ -> false
    | ElemPtr (e1, t1, es1), ElemPtr (e2, t2, es2) ->
      equal_expr e1 e2 && equal_type t1 t2 && List.equal equal_expr es1 es2
    | ElemPtr _, _ -> false
    | Malloc e1, Malloc e2 -> equal_expr e1 e2
    | Malloc _, _ -> false
    | FuncRes f1, FuncRes f2 -> equal_func f1 f2
    | FuncRes _, _ -> false
    | Exn e1, Exn e2 -> equal_expr e1 e2
    | Exn _, _ -> false
  ;;

  let expr_operand (i : instr) (idx : int) : expr =
    expr_of_llvalue (operand i idx)
  ;;

  let is_expr_null (e : expr) : bool =
    match e with
    | Var v -> LL.is_null v
    | _ -> false
  ;;

  let is_expr_global (e : expr) : bool =
    match e with
    | Var v -> is_llvalue_global v
    | _ -> false
  ;;

  let is_expr_function (e : expr) : bool =
    match e with
    | Var v -> is_llvalue_function v
    | _ -> false
  ;;

  let is_expr_func_res (e : expr) : bool =
    match e with
    | FuncRes _ -> true
    | _ -> false
  ;;

  let is_expr_undef (e : expr) : bool =
    match e with
    | Var v -> is_llvalue_undef v
    | _ -> false
  ;;

  let is_expr_deref (e : expr) : bool =
    match e with
    | Deref _ -> true
    | _ -> false
  ;;

  let is_expr_elemptr (e : expr) : bool =
    match e with
    | ElemPtr _ -> true
    | _ -> false
  ;;

  let is_expr_malloc (e : expr) : bool =
    match e with
    | Malloc _ -> true
    | _ -> false
  ;;

  let is_expr_var (e : expr) : bool =
    match e with
    | Var v -> true
    | _ -> false
  ;;

  let is_expr_int64_const (e : expr) : bool =
    match e with
    | Int64 _ -> true
    | _ -> false
  ;;

  let is_expr_float_const (e : expr) : bool =
    match e with
    | Float _ -> true
    | _ -> false
  ;;

  let is_expr_const (e : expr) : bool =
    match e with
    | Int64 _ | Float _ -> true
    | _ -> false
  ;;

  let is_zero_expr (e : expr) : bool =
    match e with
    | Int64 i -> Int64.( = ) i Int64.zero
    | _ -> false
  ;;

  let is_symbolic_expr (e : expr) : bool =
    match e with
    | Int64 _ | Float _ -> false
    | Var v -> not (LL.is_constant v)
    | _ -> false
  ;;

  let rec get_expr_depth (e : expr) : int =
    match e with
    | Undef _ | Int64 _ | Float _ | String _ | Var _ | OldE _ | FuncRes _ -> 1
    | Deref ne | ElemPtr (ne, _, _) | Malloc ne | Exn ne ->
      get_expr_depth ne + 1
  ;;

  let rec is_sub_expr (e : expr) ~(sub : expr) : bool =
    match e with
    | Undef _ | Int64 _ | Float _ | String _ | Var _ | OldE _ | FuncRes _ ->
      false
    | Deref ne | ElemPtr (ne, _, _) | Malloc ne | Exn ne ->
      equal_expr ne sub || is_sub_expr ne ~sub
  ;;

  let rec type_of_expr (e : expr) : datatype =
    let llcontext = LL.global_context () in
    match e with
    | Undef t -> t
    | Int64 _ -> LL.integer_type llcontext 64
    | Float _ -> LL.float_type llcontext
    | String s -> LL.type_of (LL.const_string llcontext s)
    | Var v -> LL.type_of v
    | OldE e -> type_of_expr e
    | Deref e' -> LL.element_type (type_of_expr e')
    | ElemPtr (root, rtyp, idxs) -> get_elemptr_typ rtyp idxs
    | Malloc e -> type_of_expr e
    | FuncRes f -> LL.return_type (LL.type_of (llvalue_of_func f))
    | Exn e -> type_of_expr e
  ;;
end

include Expr

(*-----------------------------------------
 * Predicates
 *-----------------------------------------*)

module Predicate = struct
  let equal_icompare (c1 : icompare) (c2 : icompare) : bool = c1 == c2
  let equal_fcompare (c1 : fcompare) (c2 : fcompare) : bool = c1 == c2

  let is_pred_true (p : predicate) : bool =
    match p with
    | PBool true -> true
    | _ -> false
  ;;

  let is_pred_false (p : predicate) : bool =
    match p with
    | PBool false -> true
    | _ -> false
  ;;

  let equal_pred_simple (p1 : predicate) (p2 : predicate) =
    match p1, p2 with
    | PBool b1, PBool b2 -> b1 == b2
    | PIcmp (cmp1, lhs1, rhs1), PIcmp (cmp2, lhs2, rhs2) ->
      cmp1 == cmp2 && equal_value lhs1 lhs2 && equal_value rhs1 rhs2
    | _ -> false
  ;;
end

include Predicate
