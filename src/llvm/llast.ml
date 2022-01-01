(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

open Dcore
module LL = Llvm
module LD = Llvm_debuginfo
module LT = LL.TypeKind
module LV = LL.ValueKind
module LO = LL.Opcode
module LTg = Llvm_target
module SP = Set.Poly

(*******************************************************************
 ** Core data structures of a program
 *******************************************************************)

module AST = struct
  (*----------------------------
   * Core LLVM data structures
   *---------------------------*)

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

  (** Opcode of instructions *)
  type opcode = LL.Opcode.t

  (** Basic block of functions *)
  type block = LL.llbasicblock

  (** Formal parameter of functions *)
  type param = Param of value

  (** Function in programs *)
  type func = Func of value

  (** Bitcode module of a program *)
  type bitcode_module = LL.llmodule

  (* Some convenient types *)

  type values = value list
  type datatypes = datatype list
  type uses = use list
  type params = param list
  type instrs = instr list
  type globals = global list
  type consts = const list
  type blocks = block list
  type funcs = func list

  (*----------------------------
   * Auxiliary data structures
   *---------------------------*)

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

  (** A [callable] is a function or a function pointer *)
  type callable =
    | ClFunc of func
    | ClFPtr of value

  (** Predicate used for path condition *)
  type predicate =
    | PBool of bool
    | PIcmp of (icompare * value * value)
    | PFcmp of (fcompare * value * value)
    | PNeg of predicate
    | PConj of predicate list
    | PDisj of predicate list

  (** A [prec_block] is a preceding block of a block *)
  type prec_block =
    { pblk_block : block;
      pblk_pathcond : predicate
    }

  (** A [succ_block] is a succeeding block of a block *)
  type succ_block =
    { sblk_block : block;
      sblk_pathcond : predicate
    }

  (** A [loop] captures necessary information related to a loop structure
      in programs *)
  type loop =
    { loop_head : block;
      loop_body : block list;
      loop_exit : block list;
      loop_exit_reachables : block list;
      loop_inners : loop list;
      loop_outers : loop option
    }

  (* Some convenient types *)

  type loops = loop list
  type exprs = expr list
  type callables = callable list
  type prec_blocks = prec_block list
  type succ_blocks = succ_block list

  (*----------------------------------------------
   * Modules of data structures used for Hashtbl
   *---------------------------------------------*)

  module TypeKey = struct
    type t = datatype

    let to_string t = Llvm.string_of_lltype t
    let hash = Hashtbl.hash
    let sexp_of_t t = Sexp.of_string (to_string t)
    let compare = Poly.compare
  end

  module ValueKey = struct
    module T = struct
      type t = value

      let to_string v = Llvm.string_of_llvalue v
      let hash = Hashtbl.hash
      let sexp_of_t v = Sexp.of_string (to_string v)
      let compare = Poly.compare
    end

    include T
    include Comparator.Make(T)

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

  (*------------------------
   * more modules for LLIR
   *-----------------------*)

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

  (*-------------------------
   * Program data structure
   *------------------------*)

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
      prog_bitcode_module : bitcode_module
    }
end

include AST

(*******************************************************************
 * LLVM types
 *******************************************************************)

module Type = struct
  let equal_type (t1 : datatype) (t2 : datatype) : bool = t1 == t2

  let pr_typekind (k : LT.t) : string =
    match k with
    | LT.Void -> "Void"
    | LT.Half -> "Half"
    | LT.Float -> "Float"
    | LT.Double -> "Double"
    | LT.X86fp80 -> "X86fp80"
    | LT.Fp128 -> "Fp128"
    | LT.Ppc_fp128 -> "Ppc_fp128"
    | LT.Label -> "Label"
    | LT.Integer -> "Integer"
    | LT.Function -> "Function"
    | LT.Struct -> "Struct"
    | LT.Array -> "Array"
    | LT.Pointer -> "Pointer"
    | LT.Vector -> "Vector"
    | LT.Metadata -> "Metadata"
    | LT.X86_mmx -> "X86_mmx"
    | LT.Token -> "Token"
    | LT.ScalableVector -> "ScalableVector"
    | LT.BFloat -> "BFloat"
    | LT.X86_amx -> "X86_amx"
  ;;

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

  let size_of_type (typ : datatype) (layout : LTg.DataLayout.t) : int64 =
    LTg.DataLayout.store_size typ layout
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
        | _ -> errorp "get_elemptr_typ: need to handle type: " pr_type typ
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

(*******************************************************************
 * LLVM Value
 *******************************************************************)

(** Module contains operations over value *)
module Value = struct
  let equal_value (v1 : value) (v2 : value) : bool = v1 == v2

  let pr_valuekind (k : LV.t) : string =
    match k with
    | LV.NullValue -> "NullValue"
    | LV.Argument -> "Argument"
    | LV.BasicBlock -> "BasicBlock"
    | LV.InlineAsm -> "InlineAsm"
    | LV.MDNode -> "MDNode"
    | LV.MDString -> "MDString"
    | LV.BlockAddress -> "BlockAddress"
    | LV.ConstantAggregateZero -> "ConstantAggregateZero"
    | LV.ConstantArray -> "ConstantArray"
    | LV.ConstantDataArray -> "ConstantDataArray"
    | LV.ConstantDataVector -> "ConstantDataVector"
    | LV.ConstantExpr -> "ConstantExpr"
    | LV.ConstantFP -> "ConstantFP"
    | LV.ConstantInt -> "ConstantInt"
    | LV.ConstantPointerNull -> "ConstantPointerNull"
    | LV.ConstantStruct -> "ConstantStruct"
    | LV.ConstantVector -> "ConstantVector"
    | LV.Function -> "Function"
    | LV.GlobalAlias -> "GlobalAlias"
    | LV.GlobalIFunc -> "GlobalIFunc"
    | LV.GlobalVariable -> "GlobalVariable"
    | LV.UndefValue -> "UndefValue"
    | LV.PoisonValue -> "PoisonValue"
    | LV.Instruction _ -> "Instruction"
  ;;

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

(*******************************************************************
 * Global
 *******************************************************************)

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
    | _ -> errorp "mk_global: not a global variable: " pr_value_detail v
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

  (*** Utilities ***)

  let global_name (g : global) : string = pr_global ~detailed:false g
  let global_names (gs : globals) : string = pr_args ~f:global_name gs

  let global_operand (g : global) (idx : int) : value =
    LL.operand (llvalue_of_global g) idx
  ;;

  let type_of_global (g : global) : datatype = LL.type_of (llvalue_of_global g)

  let global_succ (g : global) : global option =
    match LL.global_succ (llvalue_of_global g) with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_global v)
  ;;

  let global_pred (g : global) : global option =
    match LL.global_pred (llvalue_of_global g) with
    | LL.At_start _ -> None
    | LL.After v -> Some (mk_global v)
  ;;

  let first_global_of_module (m : bitcode_module) : global option =
    match LL.global_begin m with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_global v)
  ;;

  let last_global_of_module (m : bitcode_module) : global option =
    match LL.global_end m with
    | LL.At_start _ -> None
    | LL.After v -> Some (mk_global v)
  ;;
end

include Global

(*******************************************************************
 * Constants
 *******************************************************************)

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
    | _ -> errorp "mk_const: not a constant: " pr_value_detail v
  ;;

  (*** Conversions ***)

  let llvalue_of_const (c : const) : value =
    match c with
    | Constant v -> v
  ;;

  let int64_of_const (v : value) : int64 option = LL.int64_of_const v
  let float_of_const (v : value) : float option = LL.float_of_const v
  let string_of_const (v : value) : string option = LL.string_of_const v
end

include Const

(*******************************************************************
 * Pointer
 *******************************************************************)

module Pointer = struct
  (*-------------------------------------------------------------
   * This module contains utilities functions handling pointers
   *------------------------------------------------------------*)

  let is_pointer_to_array (ptr : value) : bool = is_type_array (LL.type_of ptr)

  (** Return the array length if [ptr] points to an array *)
  let compute_array_length (ptr : value) : int option =
    let typ = LL.type_of ptr in
    if is_type_array typ
    then (
      let elem_typ = LL.element_type (LL.type_of ptr) in
      match LL.classify_type elem_typ with
      | LL.TypeKind.Array -> Some (LL.array_length elem_typ)
      | _ -> None)
    else None
  ;;
end

include Pointer

(*******************************************************************
 * Instruction
 *******************************************************************)

module Instr = struct
  let equal_instr (i1 : instr) (i2 : instr) : bool =
    match i1, i2 with
    | Instr v1, Instr v2 -> equal_value v1 v2
  ;;

  let mk_instr (v : value) : instr =
    match LL.classify_value v with
    | LV.Instruction _ -> Instr v
    | _ -> errorp "mk_instr: not an instruction: " pr_value_detail v
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

  (** Get operand of an instruction, indexing starts from 0. *)
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

  (** Get the succeeding instruction of [i] *)
  let instr_succ (i : instr) : instr option =
    match LL.instr_succ (llvalue_of_instr i) with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_instr v)
  ;;

  let instr_pred (i : instr) : instr option =
    match LL.instr_pred (llvalue_of_instr i) with
    | LL.At_start _ -> None
    | LL.After v -> Some (mk_instr v)
  ;;

  let instr_begin (b : block) : instr option =
    match LL.instr_begin b with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_instr v)
  ;;

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

(*******************************************************************
 * Use
 *******************************************************************)

module Use = struct
  let use_succ (u : use) : use option = LL.use_succ u
  let use_begin (v : value) : use option = LL.use_begin v
end

include Use

(*******************************************************************
 * Opcode
 *******************************************************************)

module Opcode = struct
  let pr_opcode (op : LO.t) =
    match op with
    | LO.Invalid -> "Invalid"
    | LO.Ret -> "Ret"
    | LO.Br -> "Br"
    | LO.Switch -> "Switch"
    | LO.IndirectBr -> "IndirectBr"
    | LO.Invoke -> "Invoke"
    | LO.Invalid2 -> "Invalid2"
    | LO.Unreachable -> "Unreachable"
    | LO.Add -> "Add"
    | LO.FAdd -> "FAdd"
    | LO.Sub -> "Sub"
    | LO.FSub -> "FSub"
    | LO.Mul -> "Mul"
    | LO.FMul -> "FMul"
    | LO.UDiv -> "UDiv"
    | LO.SDiv -> "SDiv"
    | LO.FDiv -> "FDiv"
    | LO.URem -> "URem"
    | LO.SRem -> "SRem"
    | LO.FRem -> "FRem"
    | LO.Shl -> "Shl"
    | LO.LShr -> "LShr"
    | LO.AShr -> "AShr"
    | LO.And -> "And"
    | LO.Or -> "Or"
    | LO.Xor -> "Xor"
    | LO.Alloca -> "Alloca"
    | LO.Load -> "Load"
    | LO.Store -> "Store"
    | LO.GetElementPtr -> "GetElementPtr"
    | LO.Trunc -> "Trunc"
    | LO.ZExt -> "ZExt"
    | LO.SExt -> "SExt"
    | LO.FPToUI -> "FPToUI"
    | LO.FPToSI -> "FPToSI"
    | LO.UIToFP -> "UIToFP"
    | LO.SIToFP -> "SIToFP"
    | LO.FPTrunc -> "FPTrunc"
    | LO.FPExt -> "FPExt"
    | LO.PtrToInt -> "PtrToInt"
    | LO.IntToPtr -> "IntToPtr"
    | LO.BitCast -> "BitCast"
    | LO.ICmp -> "ICmp"
    | LO.FCmp -> "FCmp"
    | LO.PHI -> "PHI"
    | LO.Call -> "Call"
    | LO.Select -> "Select"
    | LO.UserOp1 -> "UserOp1"
    | LO.UserOp2 -> "UserOp2"
    | LO.VAArg -> "VAArg"
    | LO.ExtractElement -> "ExtractElement"
    | LO.InsertElement -> "InsertElement"
    | LO.ShuffleVector -> "ShuffleVector"
    | LO.ExtractValue -> "ExtractValue"
    | LO.InsertValue -> "InsertValue"
    | LO.Fence -> "Fence"
    | LO.AtomicCmpXchg -> "AtomicCmpXchg"
    | LO.AtomicRMW -> "AtomicRMW"
    | LO.Resume -> "Resume"
    | LO.LandingPad -> "LandingPad"
    | _ -> "UnknownOpcode"
  ;;
end

include Opcode

(*******************************************************************
 * Block
 *******************************************************************)

module Block = struct
  (*** Comparisons ***)

  let equal_block (b1 : block) (b2 : block) : bool = b1 == b2

  (*** Constructors ***)

  let mk_prec_block (blk : block) (pcond : predicate) : prec_block =
    { pblk_block = blk; pblk_pathcond = pcond }
  ;;

  let mk_succ_block (blk : block) (pcond : predicate) : succ_block =
    { sblk_block = blk; sblk_pathcond = pcond }
  ;;

  (*** Utilities ***)

  let block_name (b : block) : string = LL.value_name (LL.value_of_block b)
  let block_names (bs : block list) : string = pr_list ~f:block_name bs

  let equal_block_name (b1 : block) (b2 : block) : bool =
    let name1 = block_name b1 in
    let name2 = block_name b2 in
    String.equal name1 name2
  ;;

  let last_instr_of_block (b : block) : instr option =
    match LL.instr_end b with
    | LL.At_start _ -> None
    | LL.After v -> Some (mk_instr v)
  ;;

  let first_instr_of_block (b : block) : instr option =
    match LL.instr_begin b with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_instr v)
  ;;

  let is_first_instr_of_block (i : instr) : bool =
    let b = block_of_instr i in
    match first_instr_of_block b with
    | None -> false
    | Some i' -> equal_instr i i'
  ;;

  let is_entry_block_of_function (b : block) : bool =
    match LL.block_pred b with
    | LL.At_start _ -> true
    | LL.After _ -> false
  ;;

  let block_succ (b : block) : block option =
    match LL.block_succ b with
    | LL.At_end _ -> None
    | LL.Before b' -> Some b'
  ;;

  let block_pred (b : block) : block option =
    match LL.block_pred b with
    | LL.At_start _ -> None
    | LL.After b' -> Some b'
  ;;
end

include Block

(*******************************************************************
 * Parameters
 *******************************************************************)

module Param = struct
  (*** Comparisons ***)

  let equal_param (p1 : param) (p2 : param) : bool =
    match p1, p2 with
    | Param v1, Param v2 -> equal_value v1 v2
  ;;

  (*** Constructors ***)

  let mk_param (v : value) : param =
    match LL.classify_value v with
    | LV.Argument -> Param v
    | _ -> errorp "mk_param: not a formal parameter: " pr_value_detail v
  ;;

  let llvalue_of_param (p : param) : value =
    match p with
    | Param v -> v
  ;;

  let llvalues_of_params (ps : params) : values =
    List.map ~f:llvalue_of_param ps
  ;;

  (*** Printing ***)

  let pr_param ?(detailed = false) (p : param) : string =
    match p with
    | Param v -> if detailed then pr_value_detail v else pr_value v
  ;;

  let pr_params (ps : params) : string = pr_args ~f:pr_param ps

  let pr_typed_param (p : param) : string =
    match p with
    | Param v -> pr_type (LL.type_of v) ^ " " ^ pr_value v
  ;;

  (*** Utilities ***)

  let param_succ (p : param) : param option =
    match LL.param_succ (llvalue_of_param p) with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_param v)
  ;;

  let param_pred (p : param) : param option =
    match LL.param_pred (llvalue_of_param p) with
    | LL.At_start _ -> None
    | LL.After v -> Some (mk_param v)
  ;;
end

include Param

(*******************************************************************
 * Function
 *******************************************************************)

module Func = struct
  (*** Comparisons ***)

  let equal_func (f1 : func) (f2 : func) : bool =
    match f1, f2 with
    | Func v1, Func v2 -> equal_value v1 v2
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
    | _ -> errorp "func_name_and_params: not a function: " pr_value_detail v
  ;;

  let func_names (fs : func list) : string = pr_list ~f:func_name fs

  let func_params (f : func) : param list =
    let v = llvalue_of_func f in
    match LL.classify_value v with
    | LV.Function ->
      let vparams = Array.to_list (LL.params v) in
      List.map ~f:mk_param vparams
    | _ -> errorp "func_params: not a function: " pr_value_detail v
  ;;

  let func_type (f : func) : datatype =
    LL.return_type (LL.type_of (llvalue_of_func f))
  ;;

  let func_param_types (f : func) : datatype list =
    Array.to_list (LL.param_types (func_type f))
  ;;

  let func_return_type (f : func) : datatype = LL.return_type (func_type f)
  let func_of_block (blk : block) : func = mk_func (LL.block_parent blk)

  let func_of_instr (i : instr) : func =
    let vi = llvalue_of_instr i in
    func_of_block (LL.instr_parent vi)
  ;;

  let first_param_of_func (f : func) : param option =
    match LL.param_begin (llvalue_of_func f) with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_param v)
  ;;

  let last_param_of_func (f : func) : param option =
    match LL.param_end (llvalue_of_func f) with
    | LL.At_start _ -> None
    | LL.After v -> Some (mk_param v)
  ;;

  let blocks_of_func (f : func) : blocks =
    let v = llvalue_of_func f in
    LL.fold_left_blocks (fun acc blk -> acc @ [ blk ]) [] v
  ;;

  let first_block_of_func (f : func) : block option =
    match LL.block_begin (llvalue_of_func f) with
    | LL.At_end _ -> None
    | LL.Before blk -> Some blk
  ;;

  let last_block_of_func (f : func) : block option =
    match LL.block_end (llvalue_of_func f) with
    | LL.At_start _ -> None
    | LL.After blk -> Some blk
  ;;

  let is_first_block_of_func (blk : block) : bool =
    let func = func_of_block blk in
    match first_block_of_func func with
    | None -> false
    | Some b -> equal_block b blk
  ;;

  let first_instr_of_func (f : func) : instr option =
    match first_block_of_func f with
    | None -> None
    | Some b -> first_instr_of_block b
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

  let is_func_llvm_memcpy (f : func) : bool =
    let fname = func_name f in
    String.is_prefix fname ~prefix:"llvm.memcpy"
  ;;

  let is_func_llvm_memmove (f : func) : bool =
    let fname = func_name f in
    String.is_prefix fname ~prefix:"llvm.memmove"
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

  let func_succ (fn : func) : func option =
    match LL.function_succ (llvalue_of_func fn) with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_func v)
  ;;

  let func_pred (fn : func) : func option =
    match LL.function_pred (llvalue_of_func fn) with
    | LL.At_start _ -> None
    | LL.After v -> Some (mk_func v)
  ;;
end

include Func

(*******************************************************************
 * Callable: function or function pointer
 *******************************************************************)

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

(*******************************************************************
 * Loops
 *******************************************************************)

(** Module contains operations over loops *)
module Loop = struct
  (*** Comparisons ***)

  let equal_loop (lp1 : loop) (lp2 : loop) : bool =
    equal_block lp1.loop_head lp2.loop_head
  ;;

  (*** Constructors ***)

  let mk_loop ~(head : block) ~(body : block list) ~(exit : block list) =
    { loop_head = head;
      loop_body = body;
      loop_exit = exit;
      loop_exit_reachables = [];
      loop_inners = [];
      loop_outers = None
    }
  ;;

  (*** Printing ***)

  let pr_loop (l : loop) : string =
    let loop_info =
      [ "Loop: {head: " ^ block_name l.loop_head;
        "body: " ^ pr_list ~f:block_name l.loop_body;
        "exit: " ^ pr_list ~f:block_name l.loop_exit ^ "}"
      ] in
    String.concat ~sep:"; " loop_info
  ;;

  let pr_loops (ls : loop list) : string = pr_items ~f:pr_loop ls
end

include Loop

(*******************************************************************
 * Expression
 *******************************************************************)

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

  let expr_of_value (v : value) : expr =
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

  let operand_expr (i : instr) (idx : int) : expr =
    expr_of_value (operand i idx)
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

(*******************************************************************
 * Path
 *******************************************************************)

module Path = struct
  (*** Comparisons ***)

  let equal_icompare (c1 : icompare) (c2 : icompare) : bool = c1 == c2
  let equal_fcompare (c1 : fcompare) (c2 : fcompare) : bool = c1 == c2

  (*** Queries ***)

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

  (*** Constructors ***)

  let mk_pred_true () = PBool true
  let mk_pred_false () = PBool false

  let mk_pred_icmp cmp (lhs : value) (rhs : value) : predicate =
    PIcmp (cmp, lhs, rhs)
  ;;

  let mk_pred_fcmp cmp (lhs : value) (rhs : value) : predicate =
    PFcmp (cmp, lhs, rhs)
  ;;

  let negate_icmp (cmp : LL.Icmp.t) : LL.Icmp.t =
    match cmp with
    | LL.Icmp.Eq -> LL.Icmp.Ne
    | LL.Icmp.Ne -> LL.Icmp.Eq
    | LL.Icmp.Ugt -> LL.Icmp.Ule
    | LL.Icmp.Uge -> LL.Icmp.Ult
    | LL.Icmp.Ult -> LL.Icmp.Uge
    | LL.Icmp.Ule -> LL.Icmp.Ugt
    | LL.Icmp.Sgt -> LL.Icmp.Sle
    | LL.Icmp.Sge -> LL.Icmp.Slt
    | LL.Icmp.Slt -> LL.Icmp.Sge
    | LL.Icmp.Sle -> LL.Icmp.Sgt
  ;;

  let mk_pred_neg (p : predicate) : predicate =
    match p with
    | PBool b -> PBool (not b)
    | PIcmp (cmp, lhs, rhs) -> PIcmp (negate_icmp cmp, lhs, rhs)
    | PNeg p -> p
    | _ -> PNeg p
  ;;

  let mk_pred_conj (ps : predicate list) : predicate =
    let rec flatten acc ps =
      match ps with
      | [] -> acc
      | PConj gs :: nps -> flatten (flatten acc gs) nps
      | p :: nps -> flatten (acc @ [ p ]) nps in
    if List.is_empty ps
    then mk_pred_false ()
    else (
      let nps = flatten [] ps in
      if List.exists ~f:is_pred_false nps
      then mk_pred_false ()
      else (
        let nps = List.stable_dedup (List.exclude ~f:is_pred_true nps) in
        match nps with
        | [] -> mk_pred_true ()
        | [ np ] -> np
        | _ -> PConj nps))
  ;;

  let mk_pred_disj (ps : predicate list) : predicate =
    let rec flatten acc ps =
      match ps with
      | [] -> acc
      | PDisj gs :: nps -> flatten (flatten acc gs) nps
      | p :: nps -> flatten (acc @ [ p ]) nps in
    if List.is_empty ps
    then mk_pred_true ()
    else (
      let nps = flatten [] ps in
      if List.exists ~f:is_pred_true nps
      then mk_pred_true ()
      else (
        let nps = List.stable_dedup (List.exclude ~f:is_pred_false nps) in
        match nps with
        | [] -> mk_pred_false ()
        | [ np ] -> np
        | _ -> PDisj nps))
  ;;

  let block_of_prec_block (pblk : prec_block) : block = pblk.pblk_block
  let block_of_succ_block (sblk : succ_block) : block = sblk.sblk_block
end

include Path

(*******************************************************************
 ** Utility functions for processing metadata
 *******************************************************************)

module Metadata = struct
  let get_source_file_name (m : bitcode_module) : string =
    LL.source_file_name m
  ;;
end

include Metadata

(*******************************************************************
 * Module
 *******************************************************************)

module Module = struct
  (*** Utilities ***)

  let first_func_of_module (m : bitcode_module) : func option =
    match LL.function_begin m with
    | LL.At_end _ -> None
    | LL.Before v -> Some (mk_func v)
  ;;

  let last_func_of_module (m : bitcode_module) : func option =
    match LL.function_end m with
    | LL.At_start _ -> None
    | LL.After v -> Some (mk_func v)
  ;;
end

include Module

(*******************************************************************
 * Programs
 *******************************************************************)

module Program = struct
  let mk_program_meta_data (filename : string) (modul : bitcode_module)
      : program_meta_data
    =
    { pmd_bitcode_filename = filename;
      pmd_source_filename = get_source_file_name modul;
      pmd_llvalue_original_name = Hashtbl.create (module String);
      pmd_data_layout = LL.data_layout modul;
      pmd_target_platform = LL.target_triple modul
    }
  ;;

  let mk_program_func_data (modul : bitcode_module) : program_func_data =
    { pfd_return_instr = Hashtbl.create (module FuncKey);
      pfd_callers = Hashtbl.create (module FuncKey);
      pfd_callees = Hashtbl.create (module FuncKey);
      pfd_reachable_funcs = Hashtbl.create (module FuncKey);
      pfd_loops = Hashtbl.create (module FuncKey);
      pfd_used_globals = Hashtbl.create (module FuncKey);
      pfd_func_call_graph = CG.create ();
      pfd_block_graph = Hashtbl.create (module FuncKey);
      pfd_funcs_of_pointer = Hashtbl.create (module ValueKey);
      pfd_funcs_of_type = Hashtbl.create (module TypeKey)
    }
  ;;

  let mk_program_loop_data (modul : bitcode_module) : program_loop_data =
    { pld_loop_updated_instr = Hashtbl.create (module InstrKey);
      pld_loop_head_instr = Hashtbl.create (module InstrKey);
      pld_innermost_loop_containing_block = Hashtbl.create (module BlockKey);
      pld_innermost_loop_containing_value = Hashtbl.create (module ValueKey)
    }
  ;;

  let mk_program_block_data (modul : bitcode_module) : program_block_data =
    { pbd_preceding_blocks = Hashtbl.create (module BlockKey);
      pbd_succeeding_blocks = Hashtbl.create (module BlockKey);
      pbd_incoming_pathcond = Hashtbl.create (module BlockKey);
      pbd_reachable_blocks = Hashtbl.create (module BlockKey)
    }
  ;;

  let mk_raw_program (filename : string) (modul : bitcode_module) : program =
    let globals =
      LL.fold_left_globals (fun acc g -> acc @ [ mk_global g ]) [] modul in
    { prog_globals = globals;
      prog_struct_types = [];
      prog_all_funcs = [];
      prog_discover_funcs = [];
      prog_lib_no_source_funcs = [];
      prog_lib_has_source_funcs = [];
      prog_user_funcs = [];
      prog_init_funcs = [];
      prog_entry_funcs = [];
      prog_meta_data = mk_program_meta_data filename modul;
      prog_func_data = mk_program_func_data modul;
      prog_loop_data = mk_program_loop_data modul;
      prog_block_data = mk_program_block_data modul;
      prog_bitcode_module = modul
    }
  ;;

  let get_data_layout (prog : program) : LTg.DataLayout.t =
    LTg.DataLayout.of_string (LL.data_layout prog.prog_bitcode_module)
  ;;
end

include Program
