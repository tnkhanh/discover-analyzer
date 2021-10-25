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


module LL = Llvm
module LT = LL.TypeKind
module LV = LL.ValueKind
module LO = LL.Opcode
module SP = Set.Poly

(*******************************************************************
 ** alias to existing data structures of LLVM
 *******************************************************************)

type llvalue = LL.llvalue
type lluse = LL.lluse
type lltype = LL.lltype
type llblock = LL.llbasicblock
type llmodule = LL.llmodule

type llicmp = LL.Icmp.t
type llfcmp = LL.Fcmp.t

type llvalues = llvalue list
type lltypes = lltype list
type llmodules = llmodule list

(*******************************************************************
 ** structured types to wrap the default LLVM data structure
 *******************************************************************)

type func = Func of llvalue            (* function or function pointer *)

type block = llblock

type param = Param of llvalue          (* formal parameter of function *)

type instr = Instr of llvalue          (* instruction *)

type global = Global of llvalue        (* global variable *)

type const = Constant of llvalue       (* constant expression *)

type expr =                            (* expression *)
  | Undef of lltype
  | Int64 of int64
  | Float of float
  | String of string
  | Var of llvalue
  | OldE of expr
  | Deref of expr
  | ElemPtr of (expr * lltype * expr list)
  | Malloc of expr                     (* allocation *)
  | FuncRes of func                    (* returned value of a function *)
  | Exn of expr

type funcs = func list
type blocks = block list
type params = param list
type instrs = instr list
type globals = global list
type consts = const list
type exprs = expr list

(*******************************************************************
 ** more data structures for LLIR
 *******************************************************************)

type predicate =
  | PBool of bool
  | PIcmp of (llicmp * llvalue * llvalue)
  | PFcmp of (llfcmp * llvalue * llvalue)
  | PNeg of predicate
  | PConj of predicate list
  | PDisj of predicate list


type prec_block = {               (* preceding block *)
  pblk_block : block;
  pblk_pathcond : predicate;
}


type succ_block = {               (* succeeding block *)
  sblk_block : block;
  sblk_pathcond : predicate;
}


type prec_blocks = prec_block list
type succ_blocks = succ_block list

type loop = {
  loop_head: block;
  loop_body: block list;
  loop_exit: block list;
  loop_exit_reachables: block list;
  loop_inners: loop list;
  loop_outers: loop option;
}

type loops = loop list

(*******************************************************************
 ** wrapping modules for LLVM data structures
 *******************************************************************)

module Lltype = struct
  type t = lltype

  let to_string = fun _ -> "(lltype)"

  let hash = Hashtbl.hash
  let sexp_of_t = fun _ -> Sexp.of_string "(lltype)"
  let compare = Poly.compare
end


module Llvalue = struct

  type t = llvalue

  let to_string = fun _ -> "(llvalue)"

  let hash = Hashtbl.hash
  let sexp_of_t = fun _ -> Sexp.of_string "(llvalue)"
  let compare = Poly.compare
end


module Instr = struct

  type t = instr

  let to_string = fun _ -> "(instr)"

  let hash = Hashtbl.hash
  let sexp_of_t = fun _ -> Sexp.of_string "(instr)"
  let compare = Poly.compare
end


module Global = struct

  type t = global

  let to_string = fun _ -> "(global)"

  let hash = Hashtbl.hash
  let sexp_of_t = fun _ -> Sexp.of_string "(global)"
  let compare = Poly.compare
end


module Func = struct

  type t = func

  let to_string = fun _ -> "(func)"

  let hash = Hashtbl.hash
  let sexp_of_t = fun _ -> Sexp.of_string "(func)"
  let compare = Poly.compare
end


module Expr = struct

  type t = expr

  let to_string = fun _ -> "(expr)"

  let hash = Hashtbl.hash
  let sexp_of_t = fun _ -> Sexp.of_string "(expr)"
  let compare = Poly.compare
end


module Block = struct

  type t = llblock

  let to_string = fun _ -> "(block)"

  let hash = Hashtbl.hash
  let sexp_of_t = fun _ -> Sexp.of_string "(block)"
  let compare = Poly.compare
end

(*******************************************************************
 ** more modules for LLIR
 *******************************************************************)

module CallGraph = struct
  module Vertex = struct
    type t = func
    let compare = Poly.compare
    let equal f1 f2 = match f1, f2 with
      | Func v1, Func v2 -> v1 == v2
    let hash = Hashtbl.hash
  end

  include Graph.Imperative.Digraph.Concrete(Vertex)
end


module BlockGraph = struct

  type label =
    | NextBlock


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

  module G = Graph.Imperative.Digraph.Concrete(Vertex)
  include G

  module Weight = struct

    type edge = G.E.t
    type t = int

    let weight = fun _ -> 1

    let compare = Int.compare
    let add = Int.(+)
    let zero = 0
  end

  module Dijkstra = Graph.Path.Dijkstra(G)(Weight)
end


module InstrGraph = struct

  type rvertex = llvalue

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

  module G = Graph.Imperative.Digraph.ConcreteLabeled(Vertex)(Edge)
  include G


  module Weight = struct

    type edge = G.E.t
    type t = int

    let weight = fun _ -> 1

    let compare = Int.compare
    let add = Int.(+)
    let zero = 0
  end


  module Dijkstra = Graph.Path.Dijkstra(G)(Weight)

  (* module Pathcheck = Graph.Path.Check(G) *)
end

module CG = CallGraph

module BG = BlockGraph
module IG = InstrGraph

(*******************************************************************
 ** Finally a data structure representing program
 *******************************************************************)

type program = {
  (*-----------------------------------------
   * core components of program
   *-----------------------------------------*)

  prog_globals : global list;
  prog_struct_types : lltype list;
  prog_lib_funcs : func list;      (* lib function, won't be analyzed *)
  prog_testing_funcs : func list;  (* testing functions of analysis passes *)
  prog_init_funcs : func list;     (* initialization functions *)
  prog_user_funcs : func list;     (* user functions, will be analyzed *)
  prog_main_func : func option;    (* main function *)

  (*-----------------------------------------
   * information of functions
   *-----------------------------------------*)

  (* mapping each user function to its used global variables *)
  prog_func_return_instr : (func, instr) Hashtbl.t;

  (* mapping a function to its callers, callees, reachable functions, loops *)
  prog_func_callers : (func, funcs) Hashtbl.t;
  prog_func_callees : (func, funcs) Hashtbl.t;
  prog_func_ptr_callees : (func, llvalues) Hashtbl.t;
  prog_func_reachables : (func, funcs) Hashtbl.t;
  prog_func_loops : (func, loops) Hashtbl.t;
  prog_func_used_globals : (func, globals) Hashtbl.t;
  mutable prog_funcs_in_pointers : (lltype, funcs) Hashtbl.t;

  (* reachability graphs *)
  prog_func_call_graph : CallGraph.t;
  prog_func_block_graph : (func, BlockGraph.t) Hashtbl.t;
  prog_instr_graph : InstrGraph.t;

  (*-----------------------------------------
   * information of blocks
   *-----------------------------------------*)

  (* mapping a block to its preceding and succeeding blocks *)
  prog_block_precedings : (block, prec_blocks) Hashtbl.t;
  prog_block_succeedings : (block, succ_blocks) Hashtbl.t;

  (* mapping a block to its path conditions *)
  prog_block_incoming_pathcond : (block, predicate list) Hashtbl.t;

  (* mapping a block to its reachable blocks *)
  prog_block_reachables : (block, block list) Hashtbl.t;

  (*-----------------------------------------
   * information of instruction
   *-----------------------------------------*)
  prog_loop_updated_instr : (instr, bool) Hashtbl.t;
  prog_loop_head_instr : (instr, bool) Hashtbl.t;


  (*-----------------------------------------
   * information of llvalue
   *-----------------------------------------*)
  prog_llvalue_innermost_loop : (llvalue, loop option) Hashtbl.t;
  prog_block_innermost_loop : (block, loop option) Hashtbl.t;

  (*-----------------------------------------
   * result of analysis passes
   *-----------------------------------------*)

  (* mapping each instruction to the undefined llvalues at its program point *)
  prog_undef_values : (instr, llvalues) Hashtbl.t;

  (* hash-table mapping each pointer to its associcated functions *)
  prog_pointer_funcs : (llvalue, funcs) Hashtbl.t;

  (*-----------------------------------------
   * source code and compilation data
   *-----------------------------------------*)

  prog_bitcode_filename : string;
  prog_source_filename : string;
  prog_llvalue_original_name : (string, string) Hashtbl.t;
  prog_module_id : string;
  prog_data_layout : string;
  prog_target_platform : string;
  prog_llmodule : llmodule;
}

exception Llvm_invalid_instr of string


(*******************************************************************
 ** global variables
 *******************************************************************)

let memseg_name_index : int ref = ref (-1)


(*******************************************************************
 ** primitive printers of LLVM data structures
 *******************************************************************)

let pr_opcode (op: LL.Opcode.t) =
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


let pr_valuekind (k: LV.t) : string =
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


let pr_typekind (k: LT.t) : string =
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


let pr_icmp (cmp: LL.Icmp.t) : string =
  match cmp with
  | LL.Icmp.Eq -> "="
  | LL.Icmp.Ne -> "!="
  | LL.Icmp.Ugt -> ">"
  | LL.Icmp.Uge -> ">="
  | LL.Icmp.Ult -> "<"
  | LL.Icmp.Ule -> "<="
  | LL.Icmp.Sgt -> ">"
  | LL.Icmp.Sge -> ">="
  | LL.Icmp.Slt -> "<"
  | LL.Icmp.Sle -> "<="


let pr_fcmp (cmp: LL.Fcmp.t) : string =
  match cmp with
  | LL.Fcmp.False -> "False"
  | LL.Fcmp.Oeq -> "=="
  | LL.Fcmp.Ogt -> ">"
  | LL.Fcmp.Oge -> ">="
  | LL.Fcmp.Olt -> "<"
  | LL.Fcmp.Ole -> "<="
  | LL.Fcmp.One -> "!="
  | LL.Fcmp.Ord -> "NaN"
  | LL.Fcmp.Uno -> "NaN"
  | LL.Fcmp.Ueq -> "=="
  | LL.Fcmp.Ugt -> ">"
  | LL.Fcmp.Uge -> ">="
  | LL.Fcmp.Ult -> "<"
  | LL.Fcmp.Ule -> "<="
  | LL.Fcmp.Une -> "!="
  | LL.Fcmp.True -> "True"


let pr_value (v: llvalue) : string =
  let vtyp = LL.type_of v in
  let vname =
    if LL.is_undef v then "undef"
    else match LL.classify_type vtyp with
      | LT.Integer ->
        (match LL.int64_of_const v with
         | Some a -> Int64.to_string a
         | None -> LL.value_name v)
      | LT.Float ->
        (match LL.float_of_const v with
         | Some a -> string_of_float a
         | None -> LL.value_name v)
      | LT.Pointer ->
        if LL.is_null v then "null"
        else LL.value_name v
      | LT.Metadata -> LL.string_of_llvalue v
      | _ -> LL.string_of_llvalue v in
  if String.is_empty vname then "<empty-name>"
  else vname


let pr_values = pr_list pr_value


let value_names (vs: llvalue list) : string =
  pr_list pr_value vs


let pr_value_detail (v: llvalue) : string =
  v |> LL.string_of_llvalue |>
  String.split_lines |>
  List.map ~f:String.strip |>
  String.concat ~sep:" "


let pr_values_detail (vs: llvalues) : string =
  pr_list pr_value_detail vs


let pr_type (t: lltype) : string =
  String.strip (Llvm.string_of_lltype t)


(*******************************************************************
 ** Constructors and Conversions of LLIR data structures
 *******************************************************************)

(* constructors *)

let mk_func (v: llvalue) : func =
  match LL.classify_value v with
  | LV.Function -> Func v
  | LV.Instruction _ -> Func v
  | LV.Argument _ -> Func v
  | _ -> Func v
(* herror "mk_func: not a function or func pointer: " pr_value_detail v *)

let mk_param (v: llvalue) : param =
  match LL.classify_value v with
  | LV.Argument -> Param v
  | _ -> herror "mk_param: not a formal parameter: " pr_value_detail v


let mk_instr (v: llvalue) : instr =
  match LL.classify_value v with
  | LV.Instruction _ -> Instr v
  | _ -> herror "mk_instr: not an instruction: " pr_value_detail v


let mk_global (v: llvalue) : global =
  match LL.classify_value v with
  | LV.GlobalVariable -> Global v
  | _ -> herror "mk_global: not a global variable: " pr_value_detail v


let mk_const (v: llvalue) : const =
  match LL.classify_value v with
  | LV.ConstantExpr -> Constant v
  | _ -> herror "mk_const: not a constant: " pr_value_detail v

(* conversions *)

let llvalue_of_global (g: global) : llvalue =
  match g with
  | Global v -> v


let llvalues_of_globals (gs: globals) : llvalues =
  List.map ~f:llvalue_of_global gs


let llvalue_of_const (c: const) : llvalue =
  match c with
  | Constant v -> v


let llvalue_of_instr (i: instr) : llvalue =
  match i with
  | Instr v -> v


let llvalue_of_func (f: func) : llvalue =
  match f with
  | Func v -> v


let llvalue_of_param (p: param) : llvalue =
  match p with
  | Param v -> v


let llvalues_of_params (ps: params) : llvalues =
  List.map ~f:llvalue_of_param ps

(* expression *)

let mk_expr_undef (typ: lltype) : expr =
  Undef typ


let mk_expr_int64 (i: int64)  : expr =
  Int64 i


let mk_expr_int64_of_int (i: int)  : expr =
  Int64 (Int64.of_int i)


let mk_expr_float (f: float)  : expr =
  Float f


let mk_expr_string (s: string)  : expr =
  String s


let mk_expr_var (v: llvalue) : expr =
  Var v


let mk_expr_deref (v: llvalue) : expr =
  Deref (mk_expr_var v)


let mk_expr_elemptr (root: expr) (rtyp: lltype) (idxs: expr list) : expr =
  ElemPtr (root, rtyp, idxs)


let mk_expr_malloc (v: llvalue) : expr =
  Malloc (mk_expr_var v)


let mk_expr_func_result (f: func) : expr =
  FuncRes f


let mk_expr_exn (tinfo: llvalue) : expr =
  Exn (mk_expr_var tinfo)


let expr_of_int32 (i: int) =
  mk_expr_int64 (Int64.of_int i)


let expr_of_llvalue (v: llvalue) : expr =
  match LL.int64_of_const v, LL.float_of_const v, LL.string_of_const v with
  | None, None, None -> mk_expr_var v
  | Some i, _, _ -> mk_expr_int64 i
  | _, Some f, _ -> mk_expr_float f
  | _, _, Some s -> mk_expr_string s


let expr_of_instr (i: instr) : expr =
  mk_expr_var (llvalue_of_instr i)


let expr_of_global (g: global) : expr =
  mk_expr_var (llvalue_of_global g)


let deref_of_global (g: global) : expr =
  mk_expr_deref (llvalue_of_global g)


let deref_of_expr (e: expr) : expr =
  match e with
  | Var v -> mk_expr_deref v
  | _ -> Deref e


let elemptr_of_global (g: global) (idxs: expr list) =
  let gtyp = LL.type_of (llvalue_of_global g) in
  mk_expr_elemptr (expr_of_global g) gtyp idxs

(* loop *)

let mk_loop ~(head: block) ~(body: block list) ~(exit: block list) =
  { loop_head = head;
    loop_body = body;
    loop_exit = exit;
    loop_exit_reachables = [];
    loop_inners = [];
    loop_outers = None; }


(*******************************************************************
 ** Equality comparison
 *******************************************************************)

let equal_llvalue (v1: llvalue) (v2: llvalue) : bool =
  v1 == v2


let equal_lltype (t1: lltype) (t2: lltype) : bool =
  t1 == t2


let equal_llblock (b1: llblock) (b2: llblock) =
  b1 == b2


let equal_llicmp (c1: llicmp) (c2: llicmp) : bool =
  c1 == c2


let equal_llfcmp (c1: llfcmp) (c2: llfcmp) : bool =
  c1 == c2


let equal_func (f1: func) (f2: func) : bool =
  match f1, f2 with
  | Func v1, Func v2 -> equal_llvalue v1 v2


let equal_block (b1: block) (b2: block) : bool =
  equal_llblock b1 b2


let equal_param (p1: param) (p2: param) : bool =
  match p1, p2 with
  | Param v1, Param v2 -> equal_llvalue v1 v2


let equal_instr (i1: instr) (i2: instr) : bool =
  match i1, i2 with
  | Instr v1, Instr v2 -> equal_llvalue v1 v2


let equal_global (g1: global) (g2: global) : bool =
  match g1, g2 with
  | Global v1, Global v2 -> equal_llvalue v1 v2


let equal_const (c1: const) (c2: const) : bool =
  match c1, c2 with
  | Constant v1, Constant v2 -> equal_llvalue v1 v2


let rec equal_expr (e1: expr) (e2: expr) : bool =
  match e1, e2 with
  | Undef t1, Undef t2 -> equal_lltype t1 t2
  | Undef _, _ -> false
  | Int64 i1, Int64 i2 -> Int64.equal i1 i2
  | Int64 _, _ -> false
  | Float f1, Float f2 -> Float.equal f1 f2
  | Float _, _ -> false
  | String s1, String s2 -> String.equal s1 s2
  | String _, _ -> false
  | Var v1, Var v2 -> equal_llvalue v1 v2
  | Var _, _ -> false
  | OldE e1, OldE e2 -> equal_expr e1 e2
  | OldE _, _ -> false
  | Deref e1, Deref e2 -> equal_expr e1 e2
  | Deref _, _ -> false
  | ElemPtr (e1, t1, es1), ElemPtr (e2, t2, es2) ->
    equal_expr e1 e2 && equal_lltype t1 t2 && List.equal equal_expr es1 es2
  | ElemPtr _, _ -> false
  | Malloc e1, Malloc e2 -> equal_expr e1 e2
  | Malloc _, _ -> false
  | FuncRes f1, FuncRes f2 -> equal_func f1 f2
  | FuncRes _, _ -> false
  | Exn e1, Exn e2 -> equal_expr e1 e2
  | Exn _, _ -> false


let equal_loop (lp1: loop) (lp2: loop) : bool =
  equal_block lp1.loop_head lp2.loop_head


(*******************************************************************
 ** printing of new structured types
 *******************************************************************)

let pr_instr (i: instr) : string =
  match i with
  | Instr v -> pr_value_detail v


let pr_param ?(detailed=false) (p: param) : string =
  match p with
  | Param v ->
    if detailed then pr_value_detail v
    else pr_value v


let pr_typed_param (p: param) : string =
  match p with
  | Param v -> (pr_type (LL.type_of v)) ^ " " ^ (pr_value v)


let pr_global ?(detailed=false) (g: global) : string =
  match g with
  | Global v ->
    if detailed then pr_value_detail v
    else pr_value v


let rec pr_expr (e: expr) : string =
  match e with
  | Undef t -> "undef"
  | Int64 i -> Int64.to_string i
  | Float f -> string_of_float f
  | String s -> "\"" ^ s ^ "\""
  | Var v -> pr_value v
  | OldE e -> (pr_expr e) ^ "'"
  | Deref e -> "*" ^ (pr_expr e)
  | ElemPtr (root, rtyp, idxs) ->
    let sidxs = idxs |> List.map ~f:pr_expr |> String.concat ~sep:"," in
    "EP(" ^ (pr_expr root) ^ "," ^ (pr_type rtyp) ^ "," ^ sidxs ^ ")"
  | Malloc e -> "Malloc(" ^ (pr_expr e) ^ ")"
  | FuncRes f -> "res_" ^ (pr_value (llvalue_of_func f))
  | Exn e -> "exn_" ^ (pr_expr e)


let pr_params (ps: params) : string =
  pr_args pr_param ps


let pr_globals (gs: globals) : string =
  pr_list pr_global gs


let pr_exprs (es: expr list) : string =
  pr_list pr_expr es


let rec pr_pred (p: predicate) : string =
  match p with
  | PBool b -> pr_bool b
  | PIcmp (cmp, lhs, rhs) -> (pr_value lhs) ^ (pr_icmp cmp) ^ (pr_value rhs)
  | PFcmp (cmp, lhs, rhs) -> (pr_value lhs) ^ (pr_fcmp cmp) ^ (pr_value rhs)
  | PNeg p -> "!" ^ (pr_pred p)
  | PConj ps -> pr_list_plain ~sep:" & " pr_pred ps
  | PDisj ps -> pr_list_plain ~sep:" | " pr_pred ps


let block_name (blk: block) : string =
  LL.value_name (LL.value_of_block blk)


let block_names (blks: block list) : string =
  pr_list block_name blks


let pr_prec_block (pblk: prec_block) : string =
  let blk, p = pblk.pblk_block, pblk.pblk_pathcond in
  "Preceding Block: { " ^ (block_name blk) ^ "; " ^ (pr_pred p) ^ "}"


let pr_prec_blocks  (pblks: prec_block list) : string =
  pr_items pr_prec_block pblks


let pr_succ_block (sblk: succ_block) : string =
  let blk, p = sblk.sblk_block, sblk.sblk_pathcond in
  "Succeeding Block: { " ^ (block_name blk) ^ "; " ^ (pr_pred p) ^ "}"

(*******************************************************************
 ** wrapping of some core functions from LLVM to LLIR
 *******************************************************************)

(* instruction *)

let instr_opcode (i: instr) : LO.t =
  LL.instr_opcode (llvalue_of_instr i)


let operand (i: instr) (idx: int) : llvalue =
  LL.operand (llvalue_of_instr i) idx


let expr_operand (i: instr) (idx: int) : expr =
  expr_of_llvalue (operand i idx)


let instr_pred (i: instr) =
  LL.instr_pred (llvalue_of_instr i)


let num_operands (i: instr) : int =
  LL.num_operands (llvalue_of_instr i)


let set_operand (i: instr) (idx: int) (opr: llvalue) : unit =
  LL.set_operand (llvalue_of_instr i) idx opr


let operands (i: instr) : llvalue list =
  let oprs = ref [] in
  for idx = 0 to (num_operands i - 1) do
    oprs := !oprs @ [operand i idx]
  done;
  !oprs


let get_branch (i: instr) =
  LL.get_branch (llvalue_of_instr i)


let block_of_instr (i: instr) : block =
  LL.instr_parent (llvalue_of_instr i)


let type_of_instr (i: instr) : lltype =
  LL.type_of (llvalue_of_instr i)


let delete_instruction (i: instr) : unit =
  LL.delete_instruction (llvalue_of_instr i)


(* global variable *)

let global_operand (g: global) (idx: int) : llvalue =
  LL.operand (llvalue_of_global g) idx


let type_of_global (g: global) : lltype =
  LL.type_of (llvalue_of_global g)


(* function *)

let func_of_block (blk: block) : func =
  mk_func (LL.block_parent blk)


let func_of_instr (i: instr) : func =
  let vi = llvalue_of_instr i in
  func_of_block (LL.instr_parent vi)


let type_of_func (f: func) : lltype =
  LL.type_of (llvalue_of_func f)


(* iteration *)

let iter_instrs ~(f: instr -> unit) (blk: block) : unit =
  let ff v = f (mk_instr v) in
  LL.iter_instrs ff blk


let iter_blocks ~(f: block -> unit) (func: func) : unit =
  LL.iter_blocks f (llvalue_of_func func)


let iter_params ~(f: param -> unit) (func: func) : unit =
  let ff v = f (mk_param v) in
  LL.iter_params ff (llvalue_of_func func)


let iter_globals ~(f: global -> unit) (m: llmodule) : unit =
  let ff v = f (mk_global v) in
  LL.iter_globals ff m


let iter_functions ~(f: func -> unit) (m: llmodule) : unit =
  let ff v = f (mk_func v) in
  LL.iter_functions ff m


(* map *)

let map_instrs ~(f: instr -> 'a) (blk: block) : 'a list =
  let ff acc v = acc @ [f (mk_instr v)] in
  LL.fold_left_instrs ff [] blk


let map_blocks ~(f: block -> 'a) (func: func) : 'a list =
  let ff acc b = acc @ [f b] in
  LL.fold_left_blocks ff [] (llvalue_of_func func)


let map_params ~(f: param -> 'a) (func: func) : 'a list =
  let ff acc v = acc @ [f (mk_param v)] in
  LL.fold_left_params ff [] (llvalue_of_func func)


let map_globals ~(f: global -> 'a) (m: llmodule) : 'a list =
  let ff acc v = acc  @ [f (mk_global v)] in
  LL.fold_left_globals ff [] m


let map_functions ~(f: func -> 'a) (m: llmodule) : 'a list =
  let ff acc v = acc @ [f (mk_func v)] in
  LL.fold_left_functions ff [] m


(* folding *)

let fold_left_instrs ~(f: 'a -> instr -> 'a)  ~(init: 'a) (blk: block) : 'a =
  let ff acc v = f acc (mk_instr v) in
  LL.fold_left_instrs ff init blk


let fold_left_blocks ~(f: 'a -> block -> 'a)  ~(init: 'a) (func: func) : 'a =
  LL.fold_left_blocks f init (llvalue_of_func func)


let fold_left_params ~(f: 'a -> param -> 'a)  ~(init: 'a) (func: func) : 'a =
  let ff acc v = f acc (mk_param v) in
  LL.fold_left_params ff init (llvalue_of_func func)


let fold_left_globals ~(f: 'a -> global -> 'a)  ~(init: 'a) (m: llmodule) : 'a =
  let ff acc v = f acc (mk_global v) in
  LL.fold_left_globals ff init m


let fold_left_functions ~(f: 'a -> func -> 'a)  ~(init: 'a) (m: llmodule) : 'a =
  let ff acc v = f acc (mk_func v) in
  LL.fold_left_functions ff init m


(*******************************************************************
 ** deep iteration functions
 *******************************************************************)

type iter_instr = instr -> unit
type iter_param = param -> unit
type iter_global = global -> unit
type iter_block = block -> unit option
type iter_func = func -> unit option


let deep_iter_instr
      ?(finstr: iter_instr option = None)
      (instr: instr) : unit =
  match finstr with
  | None -> ()
  | Some f -> f instr


let deep_iter_param
      ?(fparam: iter_param option = None)
      (param: param) : unit =
  match fparam with
  | None -> ()
  | Some f -> f param


let deep_iter_global
      ?(fglobal: iter_global option = None)
      (glob: global) : unit =
  match fglobal with
  | None -> ()
  | Some f -> f glob


let deep_iter_block
      ?(fblock: iter_block option = None)
      ?(finstr: iter_instr option = None)
      (blk: block) : unit =
  let blkname = LL.value_name (LL.value_of_block blk) in
  let iter () =
    LL.iter_instrs (fun i -> deep_iter_instr ~finstr (mk_instr i)) blk in
  let open Option.Let_syntax in
  let res =
    let%bind f = fblock in
    f blk in
  Option.value res ~default:(iter ())


let deep_iter_func
      ?(ffunc: iter_func option = None)
      ?(fparam: iter_param option = None)
      ?(fblock: iter_block option = None)
      ?(finstr: iter_instr option = None)
      (func: func) : unit =
  let iter () =
    let v = llvalue_of_func func in
    LL.iter_params (fun p -> deep_iter_param ~fparam (mk_param p)) v;
    LL.iter_blocks (deep_iter_block ~fblock ~finstr) v in
  let open Option.Let_syntax in
  let res =
    let%bind f = ffunc in
    f func in
  Option.value res ~default:(iter ())


let deep_iter_module
      ?(fglobal: iter_global option = None)
      ?(ffunc: iter_func option = None)
      ?(fparam: iter_param option = None)
      ?(fblock: iter_block option = None)
      ?(finstr: iter_instr option = None)
      (m: llmodule) : unit =
  LL.iter_globals
    (fun g -> deep_iter_global ~fglobal (mk_global g)) m;
  LL.iter_functions
    (fun f -> deep_iter_func ~ffunc ~fparam ~fblock ~finstr (mk_func f)) m


let deep_iter_program
      ?(fglobal: iter_global option = None)
      ?(ffunc: iter_func option = None)
      ?(fparam: iter_param option = None)
      ?(fblock: iter_block option = None)
      ?(finstr: iter_instr option = None)
      (prog: program) : unit =
  List.iter
    ~f:(deep_iter_global ~fglobal)
    prog.prog_globals;
  List.iter
    ~f:(deep_iter_func ~ffunc ~fparam ~fblock ~finstr)
    (prog.prog_init_funcs @ prog.prog_user_funcs)


(*******************************************************************
 ** deep folding functions
 *******************************************************************)

type 'a fold_instr = 'a -> instr -> 'a
type 'a fold_param = 'a -> param -> 'a
type 'a fold_global = 'a -> global -> 'a
type 'a fold_block = 'a -> block -> 'a option
type 'a fold_func = 'a -> func -> 'a option


let deep_fold_instr
      ?(finstr: 'a fold_instr option = None)
      (acc: 'a) (instr: instr) : 'a =
  match finstr with
  | None -> acc
  | Some f -> f acc instr


let deep_fold_param
      ?(fparam: 'a fold_param option = None)
      (acc: 'a) (param: param) : 'a =
  match fparam with
  | None -> acc
  | Some f -> f acc param


let deep_fold_global
      ?(fglobal: 'a fold_global option = None)
      (acc: 'a) (glob: global) : 'a =
  match fglobal with
  | None -> acc
  | Some f -> f acc glob


let deep_fold_block
      ?(fblock: 'a fold_block option = None)
      ?(finstr: 'a fold_instr option = None)
      (acc: 'a) (blk: block) : 'a =
  let fold () =
    LL.fold_left_instrs
      (fun acc' instr -> deep_fold_instr ~finstr acc' (mk_instr instr))
      acc blk in
  let open Option.Let_syntax in
  let res =
    let%bind f = fblock in
    f acc blk in
  Option.value res ~default:(fold ())


let deep_fold_func
      ?(ffunc: 'a fold_func option = None)
      ?(fparam: 'a fold_param option = None)
      ?(fblock: 'a fold_block option = None)
      ?(finstr: 'a fold_instr option = None)
      (acc: 'a) (func: func) : 'a =
  let fold () =
    let v = llvalue_of_func func in
    acc |> fun x ->
    LL.fold_left_params
      (fun acc' param -> deep_fold_param ~fparam acc' (mk_param param))
      x v |> fun x ->
    LL.fold_left_blocks
      (fun acc' blk -> deep_fold_block ~fblock ~finstr acc' blk)
      x v in
  let open Option.Let_syntax in
  let res =
    let%bind f = ffunc in
    f acc func in
  Option.value res ~default:(fold ())


let deep_fold_module
      ?(fglobal: 'a fold_global option = None)
      ?(ffunc: 'a fold_func option = None)
      ?(fparam: 'a fold_param option = None)
      ?(fblock: 'a fold_block option = None)
      ?(finstr: 'a fold_instr option = None)
      (acc: 'a) (m: llmodule) : 'a =
  acc |> fun x ->
  LL.fold_left_globals
    (fun acc' glob -> deep_fold_global ~fglobal acc' (mk_global glob))
    x m |> fun x ->
  fold_left_functions
    ~f:(fun acc' fn -> deep_fold_func ~ffunc ~fparam ~fblock ~finstr acc' fn)
    ~init:x m


let deep_fold_program
      ?(fglobal: 'a fold_global option = None)
      ?(ffunc: 'a fold_func option = None)
      ?(fparam: 'a fold_param option = None)
      ?(fblock: 'a fold_block option = None)
      ?(finstr: 'a fold_instr option = None)
      (acc: 'a) (prog: program) : 'a =
  acc |> fun x ->
  List.fold_left
    ~f:(deep_fold_global ~fglobal)
    ~init:x prog.prog_globals |> fun x ->
  List.fold_left
    ~f:(deep_fold_func ~ffunc ~fparam ~fblock ~finstr)
    ~init:x (prog.prog_init_funcs @ prog.prog_user_funcs)


(*******************************************************************
 ** transformation
 *******************************************************************)

let int_of_const (v: llvalue) : int option =
  match LL.int64_of_const v with
  | None -> None
  | Some i -> Int64.to_int i


let int64_of_const (v: llvalue) : int64 option =
  LL.int64_of_const v


let float_of_const (v: llvalue) : float option =
  LL.float_of_const v


let string_of_const (v: llvalue) : string option =
  LL.string_of_const v


(*******************************************************************
 ** basic queries
 *******************************************************************)

(*-----------------------------------------
 * lltype
 *-----------------------------------------*)

let equal_type (t1: lltype) (t2: lltype) =
  equal_lltype t1 t2


let is_type_void (typ: lltype) : bool =
  match LL.classify_type typ with
  | LT.Void -> true
  | _ -> false


let is_type_integer (typ: lltype) : bool =
  match LL.classify_type typ with
  | LT.Integer -> true
  | _ -> false


let is_type_array (typ: lltype) : bool =
  match LL.classify_type typ with
  | LT.Array -> true
  | _ -> false


let is_type_string (typ: lltype)  : bool =
  match LL.classify_type typ with
  | LT.Array ->
    (let elemtyp = LL.element_type typ in
     match LL.classify_type elemtyp with
     | LT.Integer -> LL.integer_bitwidth elemtyp = 8
     | _ -> false)
  | _ -> false


let is_type_struct (typ: lltype) : bool =
  match LL.classify_type typ with
  | LT.Struct -> true
  | _ -> false


let is_type_pointer (typ: lltype) : bool =
  match LL.classify_type typ with
  | LT.Pointer -> true
  | _ -> false


let rec is_type_contain_pointer (typ: lltype) : bool =
  match LL.classify_type typ with
  | LT.Pointer -> true
  | LT.Struct ->
    let elem_types = Array.to_list (LL.struct_element_types typ) in
    List.exists ~f:is_type_contain_pointer elem_types
  | _ -> false


let is_type_func (typ: lltype) : bool =
  match LL.classify_type typ with
  | LT.Function -> true
  | _ -> false


let is_type_pointer_to_func (typ: lltype) : bool =
  match LL.classify_type typ with
  | LT.Pointer -> is_type_func (LL.element_type typ)
  | _ -> false


let rec get_elemptr_typ (typ: lltype) (idxs: expr list) : lltype =
  match idxs with
  | [] -> typ
  | idx::nidxs ->
    let ntyp = match LL.classify_type typ with
      | LT.Struct ->
        let fld_idx = match idx with
          | Int64 i -> Int64.to_int_exn i
          | _ -> error "get_elemptr_typ: can't get struct field idx" in
        Array.get (LL.subtypes typ) fld_idx
      | LT.Array -> LL.element_type typ
      | LT.Pointer -> LL.element_type typ
      | _ -> herror "get_elemptr_typ: need to handle type: " pr_type typ in
    get_elemptr_typ ntyp nidxs


let rec check_equiv_type (t1: lltype) (t2: lltype) : bool =
  if equal_type t1 t2 then true
  else match LL.classify_type t1, LL.classify_type t2 with
    | LT.Struct, LT.Struct ->
      let etypes1 = LL.struct_element_types t1 in
      let etypes2 = LL.struct_element_types t2 in
      if Array.length etypes1 = Array.length etypes2 then
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
    | _ -> true


(*-----------------------------------------
 * llvalue
 *-----------------------------------------*)

let equal_value (v1: llvalue) (v2: llvalue) : bool =
  equal_llvalue v1 v2


let is_llvalue_empty_name (v: llvalue) : bool =
  String.is_empty (LL.value_name v)


let is_llvalue_void (v: llvalue) : bool =
  is_type_void (Llvm.type_of v)


let is_llvalue_undef (v: llvalue) : bool =
  match LL.classify_value v with
  | LV.UndefValue -> true
  | _ -> false


let is_llvalue_pointer (v: llvalue) : bool =
  is_type_pointer (LL.type_of v)


let is_llvalue_contain_pointer (v: llvalue) : bool =
  is_type_contain_pointer (LL.type_of v)


let is_llvalue_pointer_to_function (v: llvalue) : bool =
  let typ = LL.type_of v in
  (is_type_pointer typ) && (is_type_pointer (LL.element_type typ))


let is_llvalue_param (v: llvalue) : bool =
  match LL.classify_value v with
  | LV.Argument _ -> true
  | _ -> false


let is_llvalue_instr (v: llvalue) : bool =
  match LL.classify_value v with
  | LV.Instruction _ -> true
  | _ -> false


let is_llvalue_instr_phi (v: llvalue) : bool =
  match LL.classify_value v with
  | LV.Instruction LO.PHI -> true
  | _ -> false


let is_llvalue_callable_instr (v: llvalue) : bool =
  match LL.classify_value v with
  | LV.Instruction LO.Call -> true
  | LV.Instruction LO.Invoke -> true
  | _ -> false


let is_llvalue_instr_gep (v: llvalue) : bool =
  match LL.classify_value v with
  | LV.Instruction LO.GetElementPtr -> true
  | _ -> false


let is_local_llvalue (v: llvalue) : bool =
  is_llvalue_instr v


let is_llvalue_constant_expr (v: llvalue) : bool =
  match LL.classify_value v with
  | LV.ConstantExpr -> true
  | _ -> false


let is_llvalue_null_constant (v: llvalue) : bool =
  LL.is_null v


let is_llvalue_integer_constant (v: llvalue) : bool =
  if LL.is_constant v then
    match LL.classify_type (LL.type_of v) with
    | LT.Integer -> true
    | _ -> false
  else false


let is_llvalue_argument (v: llvalue) : bool =
  match LL.classify_value v with
  | LV.Argument -> true
  | _ -> false


let is_llvalue_global (v: llvalue) : bool =
  match LL.classify_value v with
  | LV.GlobalVariable -> true
  | _ -> false


let is_llvalue_function (v: llvalue) : bool =
  match LL.classify_value v with
  | LV.Function -> true
  | _ -> false


let type_of_llvalue (v: llvalue) : lltype =
  LL.type_of v


(*-----------------------------------------
 * expression
 *-----------------------------------------*)

let is_expr_null (e: expr) : bool =
  match e with
  | Var v -> LL.is_null v
  | _ -> false


let is_expr_global (e: expr) : bool =
  match e with
  | Var v -> is_llvalue_global v
  | _ -> false


let is_expr_function (e: expr) : bool =
  match e with
  | Var v -> is_llvalue_function v
  | _ -> false


let is_expr_func_res (e: expr) : bool =
  match e with
  | FuncRes _ -> true
  | _ -> false


let is_expr_undef (e: expr) : bool =
  match e with
  | Var v -> is_llvalue_undef v
  | _ -> false


let is_expr_deref (e: expr) : bool =
  match e with
  | Deref _ -> true
  | _ -> false


let is_expr_elemptr (e: expr) : bool =
  match e with
  | ElemPtr _ -> true
  | _ -> false


let is_expr_malloc (e: expr) : bool =
  match e with
  | Malloc _ -> true
  | _ -> false


let is_expr_var (e: expr) : bool =
  match e with
  | Var v -> true
  | _ -> false


let is_integer_expr (e: expr) : bool =
  match e with
  | Int64 _ -> true
  | _ -> false


let is_float_expr (e: expr) : bool =
  match e with
  | Int64 _ -> true
  | _ -> false


let is_constant_expr (e: expr) : bool =
  match e with
  | Int64 _ | Float _ -> true
  | _ -> false


let is_zero_expr (e: expr) : bool =
  match e with
  | Int64 i -> Int64.(=) i Int64.zero
  | _ -> false


let is_symbolic_expr (e: expr) : bool =
  match e with
  | Int64 _ | Float _ -> false
  | Var v -> not (LL.is_constant v)
  | _ -> false


let rec get_expr_depth (e: expr) : int =
  match e with
  | Undef _ | Int64 _ | Float _ | String _ | Var _ | OldE _ | FuncRes _ -> 1
  | Deref ne | ElemPtr (ne, _, _) | Malloc ne | Exn ne ->
    (get_expr_depth ne) + 1


let rec is_sub_expr (e: expr) ~(sub: expr) : bool =
  match e with
  | Undef _ | Int64 _ | Float _ | String _ | Var _ | OldE _ | FuncRes _ -> false
  | Deref ne | ElemPtr (ne, _, _) | Malloc ne | Exn ne ->
    (equal_expr ne sub) || (is_sub_expr ne ~sub)


let rec type_of_expr (e: expr) : lltype =
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


(*-----------------------------------------
 * predicate
 *-----------------------------------------*)

let is_pred_true (p: predicate) : bool =
  match p with
  | PBool true -> true
  | _ -> false


let is_pred_false (p: predicate) : bool =
  match p with
  | PBool false -> true
  | _ -> false


let equal_pred_simple (p1: predicate) (p2: predicate) =
  match p1, p2 with
  | PBool b1, PBool b2 -> b1 == b2
  | PIcmp (cmp1, lhs1, rhs1), PIcmp (cmp2, lhs2, rhs2) ->
    (cmp1 == cmp2) && (equal_value lhs1 lhs2) &&
    (equal_value rhs1 rhs2)
  | _ -> false


(*-----------------------------------------
 * global
 *-----------------------------------------*)

let global_name (g: global) : string =
  pr_value (llvalue_of_global g)


let global_names (gs: globals) : string =
  pr_args global_name gs


(*-----------------------------------------
 * instruction
 *-----------------------------------------*)

let is_instr_load (i: instr) : bool =
  match instr_opcode i with
  | LO.Load -> true
  | _ -> false


let is_instr_store (i: instr) : bool =
  match instr_opcode i with
  | LO.Store -> true
  | _ -> false


let is_instr_gep (i: instr) : bool =
  match instr_opcode i with
  | LO.GetElementPtr -> true
  | _ -> false


let is_instr_extractvalue (i: instr) : bool =
  match instr_opcode i with
  | LO.ExtractValue -> true
  | _ -> false


let is_instr_call (i: instr) : bool =
  match instr_opcode i with
  | LO.Call -> true
  | _ -> false


let is_instr_invoke (i: instr) : bool =
  match instr_opcode i with
  | LO.Invoke -> true
  | _ -> false


let is_instr_call_invoke (i: instr) : bool =
  match instr_opcode i with
  | LO.Call | LO.CallBr | LO.Invoke -> true
  | _ -> false


let is_instr_return (i: instr) : bool =
  match instr_opcode i with
  | LO.Ret -> true
  | _ -> false


let is_instr_bitcast (i: instr) : bool =
  match instr_opcode i with
  | LO.BitCast -> true
  | _ -> false


let is_instr_br_or_switch (i: instr) : bool =
  match instr_opcode i with
  | LO.Br | LO.IndirectBr | LO.Switch -> true
  | _ -> false


let is_instr_unreachable (i: instr) : bool =
  match instr_opcode i with
  | LO.Unreachable -> true
  | _ -> false


let is_instr_same_block (i1: instr) (i2: instr) : bool =
  let blk1 = block_of_instr i1 in
  let blk2 = block_of_instr i2 in
  equal_block blk1 blk2


let instr_succ (i: instr) : instr option =
  match LL.instr_succ (llvalue_of_instr i) with
  | LL.At_end _ -> None
  | LL.Before v -> Some (mk_instr v)


let instr_pred (i: instr) : instr option =
  match LL.instr_pred (llvalue_of_instr i) with
  | LL.At_start _ -> None
  | LL.After v -> Some (mk_instr v)


(*-----------------------------------------
 * function and parameters
 *-----------------------------------------*)

let func_name (f: func) : string =
  let v = llvalue_of_func f in
  match LL.classify_value v with
  | LV.Function -> LL.value_name v
  | LV.Instruction _ -> LL.value_name v
  | LV.Argument _ -> LL.value_name v
  | _ -> LL.value_name v
(* herror "func_name: not a function or pointer: " pr_value_detail v *)

let param_name (p: param) : string =
  pr_value (llvalue_of_param p)


let param_names (ps: params) : string =
  pr_args param_name ps


let func_name_and_params (f: func) : string =
  let v = llvalue_of_func f in
  match LL.classify_value v with
  | LV.Function ->
    let params = Array.to_list (LL.params v) in
    (LL.value_name v) ^ "(" ^ (pr_args pr_value_detail params) ^ ")"
  | _ -> herror "func_name_and_params: not a function: " pr_value_detail v


let func_names (fs: func list) : string =
  pr_list func_name fs


let func_params (f: func) : param list =
  let v = llvalue_of_func f in
  match LL.classify_value v with
  | LV.Function ->
    let vparams = Array.to_list (LL.params v) in
    List.map ~f:mk_param vparams
  | _ -> herror "func_params: not a function: " pr_value_detail v


let func_type (f: func) : lltype =
  LL.return_type (LL.type_of (llvalue_of_func f))


let func_param_types (f: func) : lltype list =
  Array.to_list (LL.param_types (func_type f))


let func_return_type (f: func) : lltype =
  LL.return_type (func_type f)


let blocks_of_func (f: func) : blocks =
  let v = llvalue_of_func f in
  LL.fold_left_blocks (fun acc blk -> acc @ [blk]) [] v


let is_func_free (f: func) : bool =
  String.equal (func_name f) "free"


let is_func_malloc (f: func) : bool =
  String.equal (func_name f) "malloc"


let is_func_realloc (f: func) : bool =
  String.equal (func_name f) "realloc"


let is_func_nondet (f: func) : bool =
  String.equal (func_name f) "__nondet"


let is_func_handling_exception (f: func) : bool =
  let fname = func_name f in
  (String.equal fname "__cxa_allocate_exception") ||
  (String.equal fname "__cxa_throw") ||
  (String.equal fname "__cxa_begin_catch") ||
  (String.equal fname "__cxa_end_catch") ||
  (String.equal fname "llvm.eh.typeid.for")


let is_func_memcpy (f: func) : bool =
  String.is_prefix (func_name f) ~prefix:"llvm.memcpy"


let is_func_memmove (f: func) : bool =
  String.is_prefix (func_name f) ~prefix:"llvm.memmove"


let is_func_scanf (f: func) : bool =
  let fname = func_name f in
  String.equal fname "__isoc99_scanf"


let is_func_dynamic_cast (f: func) : bool =
  String.equal (func_name f) "__dynamic_cast"


let is_func_clang_call_terminate (f: func) : bool =
  String.equal (func_name f) "__clang_call_terminate"


let is_func_cpp_new (f: func) : bool =
  String.equal (func_name f) "_Znwm"


let is_func_cpp_delete (f: func) : bool =
  String.equal (func_name f) "_ZdlPv"


let is_func_main (f: func) : bool =
  String.equal (func_name f) "main"


let is_func_llvm_debug (f: func) : bool =
  let fname = func_name f in
  (String.equal fname "llvm.dbg.declare" ||
   String.equal fname "llvm.dbg.value")


let is_func_llvm_debug_declare (f: func) : bool =
  String.equal (func_name f) "llvm.dbg.declare"


let is_func_llvm_debug_value (f: func) : bool =
  String.equal (func_name f) "llvm.dbg.value"


let is_library_function (f: func) : bool =
  List.is_empty (blocks_of_func f) ||
  is_func_free f ||
  is_func_malloc f ||
  is_func_realloc f ||
  is_func_nondet f


let is_assert_func (f: func) : bool =
  let fname = func_name f in
  (String.is_substring ~substring:__assert fname) ||
  (String.is_substring ~substring:__refute fname)


let is_alias_check_func (f: func) : bool =
  let fname = func_name f in
  (String.is_substring ~substring:__assert_no_alias fname) ||
  (String.is_substring ~substring:__assert_may_alias fname) ||
  (String.is_substring ~substring:__assert_must_alias fname) ||
  (String.is_substring ~substring:__refute_no_alias fname) ||
  (String.is_substring ~substring:__refute_may_alias fname) ||
  (String.is_substring ~substring:__refute_must_alias fname)


let is_init_func (f: func) : bool =
  let fname = func_name f in
  (String.is_prefix ~prefix:__init fname)


let is_user_func (f: func) : bool =
  let v = llvalue_of_func f in
  match LL.classify_value v with
  | LV.Function ->
    not (is_library_function f) &&
    not (is_assert_func f) &&
    not (is_init_func f)
  | _ -> false


let is_func_throw_exception (f: func) : bool =
  let fname = func_name f in
  (String.equal fname "__cxa_throw")


let is_func_begin_catch_exception (f: func) : bool =
  let fname = func_name f in
  (String.equal fname "__cxa_begin_catch")


let is_func_eh_typeid_for (f: func) : bool =
  let fname = func_name f in
  (String.equal fname "llvm.eh.typeid.for")


let is_func_pointer (f: func) : bool =
  let v = llvalue_of_func f in
  match LL.classify_value v with
  | LV.Instruction _ -> true
  | _ -> false


let is_func_real_func (f: func) : bool =
  let v = llvalue_of_func f in
  match LL.classify_value v with
  | LV.Function _ -> true
  | _ -> false


let local_vars_of_func (f: func) : instr list =
  fold_left_blocks
    ~f:(fun acc1 blk ->
         let allocas = fold_left_instrs
                         ~f:(fun acc2 instr ->
                              match instr_opcode instr with
                              | LO.Alloca -> acc2 @ [instr]
                              | _ -> acc2)
                         ~init:[] blk in
         acc1 @ allocas)
    ~init:[] f


let get_library_functions (m: llmodule) =
  fold_left_functions
    ~f:(fun acc f ->
         if is_library_function f then acc @ [f]
         else acc)
    ~init:[] m


let get_user_functions (m: llmodule) =
  fold_left_functions
    ~f:(fun acc f ->
         if is_user_func f then acc @ [f]
         else acc)
    ~init:[] m


let get_auxiliary_funcs (m: llmodule) =
  fold_left_functions
    ~f:(fun acc f ->
         if is_assert_func f then acc @ [f]
         else acc)
    ~init:[] m


let get_initilization_funcs (m: llmodule) =
  fold_left_functions
    ~f:(fun acc f ->
         if is_init_func f then acc @ [f]
         else acc)
    ~init:[] m


let get_main_function (m: llmodule) : func option =
  let main_func = ref None in
  let _ = LL.iter_functions
            (fun v ->
               let f = mk_func v in
               if is_func_main f then
                 main_func := Some f)
            m in
  !main_func


(*-----------------------------------------
 * block
 *-----------------------------------------*)

let block_name_full (blk: block) : string =
  let func = func_of_block blk in
  (func_name func) ^ "_" ^ (block_name blk)


let equal_block_name (blk1: block) (blk2: block) : bool =
  let name1 = block_name blk1 in
  let name2 = block_name blk2 in
  String.equal name1 name2


let equal_block (blk1: block) (blk2: block) : bool =
  blk1 == blk2       (* compare physical LLVM object *)


(*******************************************************************
 ** substitution
 *******************************************************************)

type substv = (llvalue * llvalue) list  (* old / new values*)

type substve = (llvalue * expr) list  (* old / new values*)

type subste = (expr * expr) list        (* old / new exprs*)


let pr_substv (sst: substv) : string =
  pr_list (pr_pair pr_value pr_value) sst


let pr_subste (sst: subste) : string =
  pr_list (pr_pair pr_expr pr_expr) sst


let init_substv () : substv = []


let init_subste () : subste = []


let extend_substv (sst: substv) oldv newv : substv =
  if List.exists ~f:(fun (a, _) -> equal_llvalue oldv a) sst then sst
  else (oldv, newv)::sst


let extend_subste (sst: subste) olde newe : subste =
  if List.exists ~f:(fun (a, _) -> equal_expr olde a) sst then sst
  else (olde, newe)::sst


let extend_substve (sst: substve) oldv newe : substve =
  if List.exists ~f:(fun (a, _) -> equal_llvalue oldv a) sst then sst
  else (oldv, newe)::sst


let mk_substv ~(oldvs: llvalue list) ~(newvs: llvalue list) : substv =
  try
    List.fold2_exn
      ~f:(fun acc ov nv -> extend_substv acc ov nv)
      ~init:[] oldvs newvs
  with _ -> []


let mk_subste ~(oldes: expr list) ~(newes: expr list) : subste =
  try
    List.fold2_exn
      ~f:(fun acc oe ne -> extend_subste acc oe ne)
      ~init:[] oldes newes
  with _ -> []


let mk_substve ~(oldvs: llvalue list) ~(newes: expr list) : substve =
  try
    List.fold2_exn
      ~f:(fun acc ov ne -> extend_substve acc ov ne)
      ~init:[] oldvs newes
  with _ -> []


let subst_value (sst: substv) (v: llvalue) : llvalue =
  let res = List.find ~f:(fun (a, b) -> equal_llvalue a v) sst in
  match res with
  | Some (_, b) -> b
  | None -> v


let subst_value_expr (sst: substve) (v: llvalue) : expr =
  let res = List.find ~f:(fun (a, b) -> equal_llvalue a v) sst in
  match res with
  | Some (_, b) -> b
  | None -> Var v


let subst_values (sst: substv) (vs: llvalue list) : llvalue list =
  List.fold_left
    ~f:(fun acc v -> acc @ [(subst_value sst v)])
    ~init:[] vs


let rec subst_expr ?(sstv: substv = []) ?(sstve: substve = [])
          ?(sste: subste = []) (e: expr) : expr =
  (* first substitute expression *)
  let res = List.find ~f:(fun (a, b) -> equal_expr a e) sste in
  match res with
  | Some (_, b) -> b
  | None ->
    (* if not successful, substitute llvalue or llvalue/expr *)
    if List.not_empty sstv then
      match e with
      | Undef _ | Int64 _ | Float _ | String _ -> e
      | Var v -> Var (subst_value sstv v)
      | OldE e -> OldE (subst_expr ~sstv ~sstve ~sste e)
      | Deref e -> Deref (subst_expr ~sstv ~sstve ~sste e)
      | ElemPtr (root, rtyp, idxs) ->
        let nroot = subst_expr ~sstv ~sstve ~sste root in
        let nidxs = List.map ~f:(subst_expr ~sstv ~sstve ~sste) idxs in
        ElemPtr (nroot, rtyp, nidxs)
      | Malloc e -> Malloc (subst_expr ~sstv ~sstve ~sste e)
      | FuncRes _ -> e
      | Exn e -> Exn (subst_expr ~sstv ~sstve ~sste e)
    else
      match e with
      | Undef _ | Int64 _ | Float _ | String _ -> e
      | Var v -> subst_value_expr sstve v
      | OldE _ -> e
      | Deref e -> Deref (subst_expr ~sstv ~sstve ~sste e)
      | ElemPtr (root, rtyp, idxs) ->
        let nroot = subst_expr ~sstv ~sstve ~sste root in
        let nidxs = List.map ~f:(subst_expr ~sstv ~sstve ~sste) idxs in
        ElemPtr (nroot, rtyp, nidxs)
      | Malloc e -> Malloc (subst_expr ~sstv ~sstve ~sste e)
      | FuncRes _ -> e
      | Exn _ -> e


(*******************************************************************
 ** operations with llvalue
 *******************************************************************)

let get_undef_values_at_instr (prog: program) (instr: instr) : llvalues =
  match Hashtbl.find prog.prog_undef_values instr with
  | Some undefs -> undefs
  | None -> []


let collect_llvalue_of_expr (e: expr) : llvalues =
  let rec collect e acc = match e with
    | Undef _ | Int64 _ | Float _ | String _ -> acc
    | Var v -> List.insert_dedup acc v ~equal:equal_llvalue
    | OldE e -> collect e acc
    | Deref e -> collect e acc
    | ElemPtr (root, _, idxs) ->
      let acc1 = collect root acc in
      List.fold ~f:(fun acc2 e -> collect e acc2) ~init:acc1 idxs
    | Malloc e -> collect e acc
    | FuncRes f ->
      List.insert_dedup acc (llvalue_of_func f) ~equal:equal_llvalue
    | Exn e -> collect e acc in
  collect e []


let collect_llvalue_of_predicate (p: predicate) : llvalues =
  let equal = equal_llvalue in
  let rec collect p = match p with
    | PBool _ -> []
    | PIcmp (_, lhs, rhs) -> List.dedup ~equal [lhs; rhs]
    | PFcmp (_, lhs, rhs) -> List.dedup ~equal [lhs; rhs]
    | PNeg p1 -> collect p1
    | PConj ps | PDisj ps ->
      List.fold_left
        ~f:(fun acc p1 -> List.concat_dedup acc (collect p1) ~equal)
        ~init:[] ps in
  collect p


let num_uses (v: llvalue) : int =
  LL.fold_left_uses (fun acc _ -> acc + 1) 0 v


let get_uses (v: llvalue) : lluse list =
  LL.fold_left_uses (fun acc u -> acc @ [u]) [] v


let get_used_values (v: llvalue) : llvalue list =
  LL.fold_left_uses (fun acc u -> acc @ [LL.used_value u]) [] v


let get_users (v: llvalue) : llvalue list =
  LL.fold_left_uses (fun acc u -> acc @ [LL.user u]) [] v



(*******************************************************************
 ** operations with predicate and path condition
 *******************************************************************)

let mk_pred_true () =
  PBool true


let mk_pred_false () =
  PBool false


let mk_pred_icmp cmp (lhs: llvalue) (rhs: llvalue) : predicate =
  PIcmp (cmp, lhs, rhs)


let mk_pred_fcmp cmp (lhs: llvalue) (rhs: llvalue) : predicate =
  PFcmp (cmp, lhs, rhs)


let negate_icmp (cmp: LL.Icmp.t) : LL.Icmp.t =
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


let mk_pred_neg (p: predicate) : predicate =
  match p with
  | PBool b -> PBool (not b)
  | PIcmp (cmp, lhs, rhs) -> PIcmp (negate_icmp cmp, lhs, rhs)
  | PNeg p -> p
  | _ -> PNeg p


let mk_pred_conj (ps: predicate list) : predicate =
  let rec flatten acc ps =
    match ps with
    | [] -> acc
    | (PConj gs)::nps -> flatten (flatten acc gs) nps
    | p::nps -> flatten (acc @ [p]) nps in
  if List.is_empty ps then mk_pred_false ()
  else
    let nps = flatten [] ps in
    if List.exists ~f:is_pred_false nps then mk_pred_false ()
    else
      let nps = List.stable_dedup (List.exclude ~f:is_pred_true nps) in
      match nps with
      | [] -> mk_pred_true ()
      | [np] -> np
      | _ -> PConj nps


let mk_pred_disj (ps: predicate list) : predicate =
  let rec flatten acc ps =
    match ps with
    | [] -> acc
    | (PDisj gs)::nps -> flatten (flatten acc gs) nps
    | p::nps -> flatten (acc @ [p]) nps in
  if List.is_empty ps then mk_pred_true ()
  else
    let nps = flatten [] ps in
    if List.exists ~f:is_pred_true nps then mk_pred_true ()
    else
      let nps = List.stable_dedup (List.exclude ~f:is_pred_false nps) in
      match nps with
      | [] -> mk_pred_false ()
      | [np] -> np
      | _ -> PDisj nps


let extract_icmp_predicate (cond: llvalue) : predicate =
  match LL.icmp_predicate cond with
  | None ->
    herror "extract_icmp_predicate: not Icmp cond: " pr_value cond
  | Some cmp ->
    let lhs, rhs = LL.operand cond 0, LL.operand cond 1 in
    mk_pred_icmp cmp lhs rhs


let extract_fcmp_predicate (cond: llvalue) : predicate =
  match LL.fcmp_predicate cond with
  | None ->
    herror "extract_fcmp_predicate: not Fcmp cond: " pr_value cond
  | Some cmp ->
    let lhs, rhs = LL.operand cond 0, LL.operand cond 1 in
    mk_pred_fcmp cmp lhs rhs


let extract_trunc_predicate (cond: llvalue) : predicate =
  match LL.instr_opcode cond with
  | LO.Trunc ->
    (* FIXME: need better handling of Trunc, maybe by unfolding? *)
    mk_pred_true ()
  | _ -> herror "extract_trunc_predicate: not a Trunc cond: " pr_value cond


let extract_zext_predicate (cond: llvalue) : predicate =
  match LL.instr_opcode cond with
  | LO.ZExt ->
    (* FIXME: need better handling of ZExt? *)
    mk_pred_true ()
  | _ -> herror "extract_zext_predicate: not a ZExt cond: " pr_value cond


let extract_br_cond_predicate (cond: llvalue) : predicate =
  match LL.classify_value cond with
  | LV.Instruction _ ->
    (match LL.instr_opcode cond with
     | LO.ICmp -> extract_icmp_predicate cond
     | LO.FCmp -> extract_fcmp_predicate cond
     | LO.Trunc -> extract_trunc_predicate cond
     | LO.ZExt -> extract_zext_predicate cond
     | _ -> mk_pred_true ())
  | _ -> mk_pred_true ()


let mk_prec_block (blk: block) (pcond: predicate) : prec_block =
  { pblk_block = blk;
    pblk_pathcond = pcond; }


let mk_succ_block (blk: block) (pcond: predicate) : succ_block =
  { sblk_block = blk;
    sblk_pathcond = pcond; }


(*******************************************************************
 ** operations with type
 *******************************************************************)

let get_struct_types (m: llmodule) : lltype list =
  let all_stypes = ref Set.Poly.empty in
  let rec collect_struct_type typ =
    if is_type_struct typ then
      match LL.struct_name typ with
      | None -> ()
      | Some _ -> all_stypes := Set.add !all_stypes typ
    else (
      let subtypes = typ |> LL.subtypes |> Array.to_list in
      List.iter ~f:collect_struct_type subtypes) in
  let fglobal = Some (fun glob ->
                       collect_struct_type (LL.type_of (llvalue_of_global glob))) in
  let finstr = Some (fun instr ->
                      collect_struct_type (LL.type_of (llvalue_of_instr instr))) in
  let _ = deep_iter_module ~finstr ~fglobal m in
  Set.to_list !all_stypes


(*******************************************************************
 ** operations with globals
 *******************************************************************)

let index_of_global_name (g: global) : int =
  let gname = pr_global g in
  if String.is_prefix gname ~prefix:"g" then
    let sindex = String.sub gname ~pos:1 ~len:(String.length gname - 1) in
    try Int.of_string sindex
    with _ -> -1
  else -1


let compare_global_by_name blk1 blk2 : int =
  let idx1, idx2 = index_of_global_name blk1, index_of_global_name blk2 in
  if idx1 < idx2 then -1
  else if idx1 > idx2 then 1
  else 0


(*******************************************************************
 ** operations with instructions
 *******************************************************************)

(* Alloca *)

let dst_of_instr_alloca (i: instr) : llvalue =
  match instr_opcode i with
  | LO.Alloca -> llvalue_of_instr i
  | _ -> herror "dst_of_instr: not an instr Alloca: " pr_instr i


(* Store *)

let src_of_instr_store (i: instr) : llvalue =
  match instr_opcode i with
  | LO.Store -> operand i 0
  | _ -> herror "src_of_instr: not an instr Store: " pr_instr i


let dst_of_instr_store (i: instr) : llvalue =
  match instr_opcode i with
  | LO.Store -> operand i 1
  | _ -> herror "dst_of_instr: not an instr Store: " pr_instr i


(* Load *)

let src_of_instr_load (i: instr) : llvalue =
  match instr_opcode i with
  | LO.Load -> operand i 0
  | _ -> herror "src_of_instr: not an instr Load: " pr_instr i


let dst_of_instr_load (i: instr) : llvalue =
  match instr_opcode i with
  | LO.Load -> llvalue_of_instr i
  | _ -> herror "dst_of_instr: not an instr Load: " pr_instr i


(* InsertValue *)

let src_of_instr_insertvalue (i: instr) : llvalue =
  match instr_opcode i with
  | LO.InsertValue -> operand i 1
  | _ -> herror "src_of_instr: not an instr InsertValue: " pr_instr i


let dst_of_instr_insertvalue (i: instr) : llvalue =
  match instr_opcode i with
  | LO.InsertValue -> operand i 0
  | _ -> herror "dst_of_instr: not an instr InsertValue: " pr_instr i


(* ExtractValue *)

let src_of_instr_extractvalue (i: instr) : llvalue =
  match instr_opcode i with
  | LO.ExtractValue -> operand i 0
  | _ -> herror "src_of_instr: not an instr ExtractValue: " pr_instr i


let dst_of_instr_extractvalue (i: instr) : llvalue =
  match instr_opcode i with
  | LO.ExtractValue -> llvalue_of_instr i
  | _ -> herror "dst_of_instr: not an instr ExtractValue: " pr_instr i


(* GetElementPointer *)

let src_of_instr_gep (i: instr) : llvalue =
  match instr_opcode i with
  | LO.GetElementPtr -> operand i 0
  | _ -> herror "src_of_instr: not an instr GEP: " pr_instr i


let dst_of_instr_gep (i: instr) : llvalue =
  match instr_opcode i with
  | LO.GetElementPtr -> llvalue_of_instr i
  | _ -> herror "dst_of_instr: not an instr GEP: " pr_instr i


let indexes_of_instr_gep (i: instr) : llvalue list =
  let indexes = ref [] in
  for idx = 1 to (num_operands i) - 1 do
    indexes := !indexes @ [operand i idx]
  done;
  !indexes


(* GetElementPointer/ExtractValue *)

let src_of_instr_gep_extract_value (i: instr) : llvalue =
  match instr_opcode i with
  | LO.GetElementPtr -> operand i 0
  | LO.ExtractValue -> operand i 0
  | _ -> herror "src_of_instr: not an instr GEP/ExtractValue: " pr_instr i


let dst_of_instr_gep_extract_value (i: instr) : llvalue =
  match instr_opcode i with
  | LO.GetElementPtr -> llvalue_of_instr i
  | LO.ExtractValue -> llvalue_of_instr i
  | _ -> herror "dst_of_instr: not an instr GEP/ExtractValue: " pr_instr i


let indexes_of_instr_gep_extract_value (i: instr) : llvalue list =
  let indexes = ref [] in
  for idx = 1 to (num_operands i) - 1 do
    indexes := !indexes @ [operand i idx]
  done;
  !indexes


(* BitCast *)

let src_of_instr_bitcast (i: instr) : llvalue =
  match instr_opcode i with
  | LO.BitCast -> operand i 0
  | _ -> herror "src_of_instr: not an instr BitCast: " pr_instr i


let dst_of_instr_bitcast (i: instr) : llvalue =
  match instr_opcode i with
  | LO.BitCast -> llvalue_of_instr i
  | _ -> herror "dst_of_instr: not an instr BitCast: " pr_instr i


let rec get_root_src_of_bitcast (v: llvalue) : llvalue =
  match LL.classify_value v with
  | LV.Instruction LO.BitCast -> get_root_src_of_bitcast (LL.operand v 0)
  | _ -> v


(* FuncRes *)

let src_of_instr_return (i: instr) : llvalue =
  match instr_opcode i with
  | LO.Ret -> operand i 0
  | _ -> herror "src_of_instr: not an instr FuncRes: " pr_instr i


(* SExt *)

let src_of_instr_sext (i: instr) : llvalue =
  match instr_opcode i with
  | LO.SExt -> operand i 0
  | _ -> herror "src_of_instr: not an instr SExt: " pr_instr i


let dst_of_instr_sext (i: instr) : llvalue = match instr_opcode i with
  | LO.SExt -> llvalue_of_instr i
  | _ -> herror "dst_of_instr: not an instr SExt: " pr_instr i


(* ZExt *)

let src_of_instr_zext (i: instr) : llvalue =
  match instr_opcode i with
  | LO.ZExt -> operand i 0
  | _ -> herror "src_of_instr: not an instr ZExt: " pr_instr i


let dst_of_instr_zext (i: instr) : llvalue = match instr_opcode i with
  | LO.ZExt -> llvalue_of_instr i
  | _ -> herror "dst_of_instr: not an instr ZExt: " pr_instr i


(* Call *)

let num_args_of_instr_call (i: instr) : int =
  match instr_opcode i with
  | LO.Call -> LL.num_arg_operands (llvalue_of_instr i)
  | _ -> herror "num_args_of: not an instr Call: " pr_instr i


let callee_of_instr_call (i: instr) : func =
  match instr_opcode i with
  | LO.Call ->
    let num_args = num_args_of_instr_call i in
    mk_func (operand i num_args)
  | _ -> herror "callee_of: not an instr Call: " pr_instr i


let arg_of_instr_call (i: instr) (idx: int) : llvalue =
  match instr_opcode i with
  | LO.Call ->
    if (idx < num_args_of_instr_call i) then operand i idx
    else herror "arg_of_instr_call: idx out of bound" pr_int idx
  | _ -> herror "arg_of: not an instr Call: " pr_instr i


let args_of_instr_call (i: instr) : llvalues =
  match instr_opcode i with
  | LO.Call ->
    let args = ref [] in
    let num_args = num_args_of_instr_call i in
    for idx = 0 to (num_args - 1) do
      args := !args @ [operand i idx]
    done;
    !args
  | _ -> herror "operand_args: not an instr Call: " pr_instr i


(* CallBr *)

let num_args_of_instr_callbr (i: instr) : int =
  match instr_opcode i with
  | LO.CallBr -> LL.num_arg_operands (llvalue_of_instr i)
  | _ -> herror "num_args_of: not an instr CallBr: " pr_instr i


let arg_of_instr_callbr (i: instr) (idx: int) : llvalue =
  match instr_opcode i with
  | LO.CallBr ->
    if (idx < num_args_of_instr_call i) then operand i idx
    else herror "arg_of_instr_call: idx out of bound" pr_int idx
  | _ -> herror "arg_of: not an instr Call: " pr_instr i


let args_of_instr_callbr (i: instr) : llvalues =
  match instr_opcode i with
  | LO.CallBr ->
    let args = ref [] in
    let num_args = num_args_of_instr_call i in
    for idx = 0 to (num_args - 1) do
      args := !args @ [operand i idx]
    done;
    !args
  | _ -> herror "operand_args: not an instr CallBr: " pr_instr i


let callee_of_instr_callbr (i: instr) : func =
  match instr_opcode i with
  | LO.CallBr ->
    let num_args = num_args_of_instr_callbr i in
    mk_func (operand i num_args)
  | _ -> herror "callee_of: not an instr CallBr: " pr_instr i


(* Invoke *)

let num_args_of_instr_invoke (i: instr) : int =
  match instr_opcode i with
  | LO.Invoke -> LL.num_arg_operands (llvalue_of_instr i)
  | _ -> herror "num_args_of: not an instr Invoke: " pr_instr i


let callee_of_instr_invoke (i: instr) : func =
  match instr_opcode i with
  | LO.Invoke ->
    let num_args = num_args_of_instr_invoke i in
    mk_func (operand i (num_args + 2))
  | _ -> herror "callee_of: not an instr Invoke: " pr_instr i


let unwind_dest_of_instr_invoke (i: instr) : block =
  match instr_opcode i with
  | LO.Invoke -> LL.get_unwind_dest (llvalue_of_instr i)
  | _ -> herror "unwind_dest_of: not an instr Invoke: " pr_instr i


let normal_dest_of_instr_invoke (i: instr) : block =
  match instr_opcode i with
  | LO.Invoke -> LL.get_normal_dest (llvalue_of_instr i)
  | _ -> herror "normal_dest_of: not an instr Invoke: " pr_instr i


let arg_of_instr_invoke (i: instr) (idx: int) : llvalue =
  match instr_opcode i with
  | LO.Invoke ->
    if (idx < num_args_of_instr_call i) then operand i idx
    else herror "arg_of_instr_invoke: idx out of bound" pr_int idx
  | _ -> herror "arg_of: not an instr Invoke: " pr_instr i


let args_of_instr_invoke (i: instr) : llvalues =
  match instr_opcode i with
  | LO.Invoke ->
    let args = ref [] in
    let num_args = num_args_of_instr_invoke i in
    for idx = 0 to (num_args - 1) do
      args := !args @ [operand i idx]
    done;
    !args
  | _ -> herror "operand_args: not an instr Invoke: " pr_instr i


(* Function application instructions are: Call, CallBr, Invoke *)

let num_args_of_instr_func_app (i: instr) : int =
  match instr_opcode i with
  | LO.Call -> num_args_of_instr_call i
  | LO.CallBr -> num_args_of_instr_call i
  | LO.Invoke -> num_args_of_instr_invoke i
  | _ ->
    herror "num_args_of_instr_func_app: not a callable instr: " pr_instr i


let callee_of_instr_func_call (i: instr) : func =
  match instr_opcode i with
  | LO.Call -> callee_of_instr_call i
  | LO.CallBr -> callee_of_instr_callbr i
  | LO.Invoke -> callee_of_instr_invoke i
  | _ ->
    herror "callee_of_instr_func_call: not a callable instr: " pr_instr i


let arg_of_instr_func_app (i: instr) (idx: int) : llvalue =
  match instr_opcode i with
  | LO.Call -> arg_of_instr_call i idx
  | LO.CallBr -> arg_of_instr_callbr i idx
  | LO.Invoke -> arg_of_instr_invoke i idx
  | _ ->
    herror "arg_of_instr_func_app: not a callable instr: " pr_instr i


let args_of_instr_func_app (i: instr) : llvalues =
  match instr_opcode i with
  | LO.Call -> args_of_instr_call i
  | LO.CallBr -> args_of_instr_callbr i
  | LO.Invoke -> args_of_instr_invoke i
  | _ ->
    herror "args_of_instr_func_app: not a callable instr: " pr_instr i


let get_origin_src_of_memcpy (i: instr) : llvalue =
  let callee = callee_of_instr_func_call i in
  if is_func_memcpy callee then
    operand (mk_instr (operand i 0)) 0
  else herror "get_origin_src_of_memcpy: not a memcopy Call: " pr_instr i


let get_origin_dst_of_memcpy (i: instr) : llvalue =
  let callee = callee_of_instr_func_call i in
  if is_func_memcpy callee then
    operand (mk_instr (operand i 1)) 0
  else herror "get_origin_dst_of_memcpy: not a memcopy Call: " pr_instr i


(* Icmp *)

let predicate_of_instr_icmp (i: instr) : LL.Icmp.t option =
  match instr_opcode i with
  | LO.ICmp -> LL.icmp_predicate (llvalue_of_instr i)
  | _ -> herror "predicate: not an instr Icmp: " pr_instr i


(* Fcmp *)

let predicate_of_instr_fcmp (i: instr) : LL.Fcmp.t option =
  match instr_opcode i with
  | LO.FCmp -> LL.fcmp_predicate (llvalue_of_instr i)
  | _ -> herror "predicate: not an instr FCmp: " pr_instr i


(* Br *)

let branch_of_instr_br (i: instr) =
  match instr_opcode i with
  | LO.Br | LO.IndirectBr -> LL.get_branch (llvalue_of_instr i)
  | _ -> herror "branch: not an instr Br: " pr_instr i


(* PHI Node *)

let src_of_instr_phi (i: instr) : llvalues =
  match instr_opcode i with
  | LO.PHI ->
    let operands = ref [] in
    for idx = 0 to (num_operands i) - 1 do
      operands := !operands @ [operand i idx]
    done;
    !operands
  | _ -> herror "operands: not an instr PHI: " pr_instr i


let src_and_origin_of_instr_phi (i: instr) : (llvalue * block) list =
  match instr_opcode i with
  | LO.PHI -> LL.incoming (llvalue_of_instr i)
  | _ -> herror "operands: not an instr PHI: " pr_instr i


let dst_of_instr_phi (i: instr) : llvalue =
  match instr_opcode i with
  | LO.PHI -> llvalue_of_instr i
  | _ -> herror "dst_of_instr: not an instr PHI: " pr_instr i


let is_phi_of_same_src_and_origin (i1: instr) (i2: instr) : bool =
  let src_origin1 = src_and_origin_of_instr_phi i1 in
  let src_origin2 = src_and_origin_of_instr_phi i2 in
  if List.length src_origin1 = List.length src_origin2 then
    List.for_all2_exn
      ~f:(fun (v1, b1) (v2, b2) ->
           (equal_value v1 v2) && (equal_block b1 b2))
      src_origin1 src_origin2
  else false


(*******************************************************************
 ** operations with blocks
 *******************************************************************)

let index_of_block_name (blk: block) : int =
  let bname = block_name blk in
  if String.is_prefix bname ~prefix:"bb" then
    let sindex = String.sub bname ~pos:2 ~len:(String.length bname - 2) in
    try Int.of_string sindex
    with _ -> -1
  else -1


let compare_block_by_name blk1 blk2 : int =
  let idx1, idx2 = index_of_block_name blk1, index_of_block_name blk2 in
  if idx1 < idx2 then -1
  else if idx1 > idx2 then 1
  else 0


let is_entry_block (blk: block) func : bool =
  equal_block blk (LL.entry_block func)


let last_instr_of_block (blk: block) : instr option =
  match LL.instr_end blk with
  | LL.At_start _ -> None
  | LL.After v -> Some (mk_instr v)


let first_instr_of_block (blk: block) : instr option =
  match LL.instr_begin blk with
  | LL.At_end _ -> None
  | LL.Before v -> Some (mk_instr v)


let is_first_instr_of_block (instr: instr) : bool =
  let blk = block_of_instr instr in
  match first_instr_of_block blk with
  | None -> false
  | Some i -> equal_instr i instr


let is_entry_block_of_function (blk: block) : bool =
  match LL.block_pred blk with
  | LL.At_start _ -> true
  | LL.After _ -> false


let block_of_prec_block (pblk: prec_block) : block =
  pblk.pblk_block


let block_of_succ_block (sblk: succ_block) : block =
  sblk.sblk_block


let get_preceding_blocks (prog: program) (blk: block) : prec_blocks =
  let compute_blocks (blk: block) : prec_blocks =
    let func = func_of_block blk in
    fold_left_blocks
      ~f:(fun acc1 blk1 ->
           fold_left_instrs
             ~f:(fun acc2 instr ->
                  match instr_opcode instr with
                  | LO.IndirectBr | LO.Br ->
                    (match get_branch instr with
                     | None  -> acc2
                     | Some (`Unconditional blk2) ->
                       if equal_block blk blk2 then
                         acc2 @ [mk_prec_block blk1 (mk_pred_true ())]
                       else acc2
                     | Some (`Conditional (cond, blk21, blk22)) ->
                       let pred = extract_br_cond_predicate cond in
                       if equal_block blk blk21 then
                         acc2 @ [mk_prec_block blk1 pred]
                       else if equal_block blk blk22 then
                         acc2 @ [mk_prec_block blk1 (mk_pred_neg pred)]
                       else acc2)
                  | LO.Switch ->
                    let pblks = ref [] in
                    for i = 0 to (num_operands instr) / 2 - 1 do
                      let blk2 = LL.block_of_value (operand instr (i * 2 + 1)) in
                      if equal_block blk blk2 then
                        pblks := [mk_prec_block blk (mk_pred_true ())]
                    done;
                    acc2 @ !pblks
                  | LO.Invoke ->
                    let blk21 = normal_dest_of_instr_invoke instr in
                    let blk22 = unwind_dest_of_instr_invoke instr in
                    if equal_block blk blk21 then
                      acc2 @ [mk_prec_block blk1 (mk_pred_true ())]
                    else if equal_block blk blk22 then
                      acc2 @ [mk_prec_block blk1 (mk_pred_true ())]
                    else acc2
                  | _ -> acc2)
             ~init:acc1 blk1)
      ~init:[] func in
  Hashtbl.find_or_compute prog.prog_block_precedings ~key:blk
    ~f:(fun () -> compute_blocks blk)


let get_succeeding_blocks (prog: program) (blk: block) : succ_blocks =
  let compute_blocks (blk: block) : succ_blocks =
    fold_left_instrs
      ~f:(fun acc instr ->
           match instr_opcode instr with
           | LO.IndirectBr | LO.Br ->
             let sblks = match get_branch instr with
               | None  -> []
               | Some (`Unconditional b) ->
                 [mk_succ_block b (mk_pred_true ())]
               | Some (`Conditional (cond, b1, b2)) ->
                 let pred = extract_br_cond_predicate cond in
                 let sblk1 = mk_succ_block b1 pred in
                 let sblk2 = mk_succ_block b2 (mk_pred_neg pred) in
                 [sblk1; sblk2] in
             acc @ sblks
           | LO.Switch ->
             let sblks = ref [] in
             for i = 0 to (num_operands instr) / 2 - 1 do
               let blk = LL.block_of_value (operand instr (i * 2 + 1)) in
               let sblk = mk_succ_block blk (mk_pred_true ()) in
               sblks := !sblks @ [sblk]
             done;
             acc @ !sblks
           | LO.Invoke ->
             let blk1 = normal_dest_of_instr_invoke instr in
             let blk2 = unwind_dest_of_instr_invoke instr in
             let sblk1 = mk_succ_block blk1 (mk_pred_true ()) in
             let sblk2 = mk_succ_block blk2 (mk_pred_true ()) in
             acc @ [sblk1; sblk2]
           | _ -> acc) ~init:[] blk in
  Hashtbl.find_or_compute prog.prog_block_succeedings ~key:blk
    ~f:(fun () -> compute_blocks blk)


let has_unique_path_between_blocks prog (src: block) (dst: block) : bool =
  let rec check_path blk =
    if equal_block blk dst then true
    else match  get_succeeding_blocks prog blk with
      | [sblk] -> check_path sblk.sblk_block
      |  _ -> false in
  check_path src


let get_succeeding_only_blocks (prog: program) (blk: block) : blocks =
  blk |> get_succeeding_blocks prog |>
  List.map ~f:(fun sb -> sb.sblk_block)


let get_pathcond_between_blocks prog (src: block) (dst: block) : predicate option =
  let sblks = get_succeeding_blocks prog src in
  let sblks = List.filter
                ~f:(fun sblk -> equal_block sblk.sblk_block dst)
                sblks in
  match sblks with
  | [] -> None
  | sblk::_ -> Some sblk.sblk_pathcond


(*******************************************************************
 ** operations with functions and parameters
 *******************************************************************)

(* formal parameters *)
let formal_params_of_func (f: func) : param list =
  let v = llvalue_of_func f in
  match LL.classify_value v with
  | LV.Function -> fold_left_params ~f:(fun acc p -> acc @ [p]) ~init:[] f
  | _ -> herror "formal_params_of_func: not an actual function: " pr_value v


let entry_block (f: func) : block =
  LL.entry_block (llvalue_of_func f)


let blocks_of_func (f: func) : blocks =
  fold_left_blocks ~f:(fun acc blk -> acc @ [blk]) ~init:[] f


let delete_function (f: func) : unit =
  LL.delete_function (llvalue_of_func f)


let first_block_of_func (f: func) : block option =
  match LL.block_begin (llvalue_of_func f) with
  | LL.At_end _ -> None
  | LL.Before blk -> Some blk


let last_block_of_func (f: func) : block option =
  match LL.block_end (llvalue_of_func f) with
  | LL.At_start _ -> None
  | LL.After blk -> Some blk


let is_first_block_of_func (blk: block) : bool =
  let func = func_of_block blk in
  match first_block_of_func func with
  | None -> false
  | Some b -> equal_block b blk


let first_instr_of_func (f: func) : instr option =
  match first_block_of_func f with
  | None -> None
  | Some b -> first_instr_of_block b


let get_func_callees (prog: program) (f: func) : funcs =
  match Hashtbl.find prog.prog_func_callees f with
  | None -> []
  | Some fns -> fns


let get_func_ptr_callees (prog: program) (f: func) : llvalues =
  match Hashtbl.find prog.prog_func_ptr_callees f with
  | None -> []
  | Some vs -> vs


let get_func_callers (prog: program) (f: func) : funcs =
  match Hashtbl.find prog.prog_func_callers f with
  | None -> []
  | Some fns -> fns


let get_func_used_globals prog (f: func) : globals =
  if is_user_func f || is_init_func f then
    match Hashtbl.find prog.prog_func_used_globals f with
    | None -> herror "get_func_used_globals: no infor of: " func_name f
    | Some gs -> gs
  else []


let has_call_to_user_funcs prog (f: func) : bool =
  let callees = get_func_callees prog f in
  List.exists ~f:(fun f -> is_user_func f || is_func_pointer f) callees


(*******************************************************************
 ** more utility operations
 *******************************************************************)

(** TRUNG: this function might be inefficient if it is used to compute
    reachable blocks of all blocks.
    TODO: need to think how to re-use the result of a succeeding block
    when computing the reachability of a preceding block.*)

let get_reachable_blocks (prog: program) (blk: block) : blocks =
  let rec compute_reachables (queue: blocks) (visited: blocks) =
    match queue with
    | [] -> visited
    | blk::nqueue ->
      let nblks = blk |> get_succeeding_blocks prog |>
                  List.map ~f:(fun sblk -> sblk.sblk_block) |>
                  List.exclude ~f:(List.mem ~equal:(==) visited) |>
                  List.exclude ~f:(List.mem ~equal:(==) nqueue) in
      let nqueue = nqueue @ nblks in
      let nvisited = visited @ [blk] in
      compute_reachables nqueue nvisited in
  let compute () =
    let sblks = blk |> get_succeeding_blocks prog |>
                List.map ~f:(fun sb -> sb.sblk_block) in
    compute_reachables sblks [] in
  Hashtbl.find_or_compute prog.prog_block_reachables ~key:blk ~f:compute


(* TODO: can be optimized by compute for all functions at once *)
let get_reachable_funcs (prog: program) (f: func) : funcs =
  let equal = equal_func in
  let rec compute_reachables (queue: func list) (visited: funcs) =
    match queue with
    | [] -> visited
    | f::nqueue ->
      let callees = get_func_callees prog f in
      let nfs = List.diff callees (nqueue @ visited) ~equal in
      let nqueue = List.concat_dedup nqueue nfs ~equal in
      let nvisited = List.insert_dedup visited f ~equal in
      compute_reachables nqueue nvisited in
  let compute () =
    let callees = get_func_callees prog f in
    compute_reachables callees [] in
  Hashtbl.find_or_compute prog.prog_func_reachables ~key:f ~f:compute


let is_reachable_func prog (src: func) (dst: func) : bool =
  let funcs = get_reachable_funcs prog src in
  List.mem funcs dst ~equal:equal_func


let is_reachable_block prog (src: block) (dst: block) : bool =
  let func1, func2 = func_of_block src, func_of_block dst in
  if equal_func func1 func2 then
    let blks = get_reachable_blocks prog src in
    List.mem blks dst ~equal:equal_block
  else false


let is_reachable_instr prog (src: instr) (dst: instr) : bool =
  let rec check_reachable_instr_same_block instr1 instr2 =
    if equal_instr instr1 instr2 then true
    else match instr_succ instr1 with
      | None -> false
      | Some instr' -> check_reachable_instr_same_block instr' instr2 in
  let blk1, blk2 = block_of_instr src, block_of_instr dst in
  if equal_block blk1 blk2 then
    check_reachable_instr_same_block src dst
  else is_reachable_block prog blk1 blk2


(*******************************************************************
 ** operations with programs
 *******************************************************************)

let find_user_func (prog: program) (fname: string) : func option =
  List.find
    ~f:(fun func -> String.equal (func_name func) fname)
    prog.prog_user_funcs


let get_current_funcs_of_pointer (prog: program) (v: llvalue) : funcs =
  match Hashtbl.find prog.prog_pointer_funcs v with
  | None -> []
  | Some funcs -> funcs


let update_funcs_of_pointer (prog: program) (v: llvalue) (funcs: funcs) =
  let curr_funcs = get_current_funcs_of_pointer prog v in
  let new_funcs = List.concat_dedup curr_funcs funcs ~equal:equal_func in
  let _ = hdebug "update func pointer of: " pr_value v in
  let _ = hdebug "   new funcs: " func_names new_funcs in
  Hashtbl.set prog.prog_pointer_funcs ~key:v ~data:new_funcs


(*******************************************************************
 ** constructors of program
 *******************************************************************)

let mk_program (filename: string) (m: llmodule) : program =
  let compiled_target = LL.target_triple m in
  let data_layout = LL.data_layout m in
  let globals = LL.fold_left_globals (fun acc g -> acc @ [mk_global g]) [] m in
  (* let module_id, source_filename =
   *   let str = LL.string_of_llmodule m in
   *   let re = Str.regexp ".*ModuleID.*'\\(.*\\\)'\nsource_filename.=.\"\\(.*\\\)\"" in
   *   if Str.string_match re str 0 then
   *     (Str.matched_group 1 str, Str.matched_group 2 str)
   *   else ("unknown", "unknown") in *)
  { prog_globals = globals;
    prog_struct_types = []; (* get_struct_types m; *)
    prog_lib_funcs = get_library_functions m;
    prog_user_funcs = get_user_functions m;
    prog_testing_funcs = get_auxiliary_funcs m;
    prog_init_funcs = get_initilization_funcs m;
    prog_main_func = get_main_function m;
    (* functions info *)
    prog_func_return_instr = Hashtbl.create (module Func);
    prog_func_callees = Hashtbl.create (module Func);
    prog_func_ptr_callees = Hashtbl.create (module Func);
    prog_func_callers = Hashtbl.create (module Func);
    prog_func_reachables = Hashtbl.create (module Func);
    prog_func_loops = Hashtbl.create (module Func);
    prog_func_used_globals = Hashtbl.create (module Func);
    prog_funcs_in_pointers = Hashtbl.create (module Lltype);
    prog_func_call_graph = CG.create ();
    prog_func_block_graph = Hashtbl.create (module Func);
    prog_instr_graph = IG.create();
    (* blocks info *)
    prog_block_precedings = Hashtbl.create (module Block);
    prog_block_succeedings = Hashtbl.create (module Block);
    prog_block_incoming_pathcond = Hashtbl.create (module Block);
    prog_block_reachables = Hashtbl.create (module Block);
    (* instruction info *)
    prog_loop_updated_instr = Hashtbl.create (module Instr);
    prog_loop_head_instr = Hashtbl.create (module Instr);
    (* llvalue info *)
    prog_llvalue_innermost_loop = Hashtbl.create (module Llvalue);
    prog_block_innermost_loop = Hashtbl.create (module Block);
    (* analysis results *)
    prog_undef_values = Hashtbl.create (module Instr);
    prog_pointer_funcs = Hashtbl.create (module Llvalue);
    (* compilation info *)
    prog_bitcode_filename = filename;
    prog_source_filename = "<unknown>"; (* source_filename; *)
    prog_llvalue_original_name = Hashtbl.create (module String);
    prog_module_id = "<unknown>"; (* module_id; *)
    prog_data_layout = data_layout;
    prog_target_platform = compiled_target;
    prog_llmodule = m; }


(*******************************************************************
 ** more advanced printing
 *******************************************************************)

let pr_loop (l: loop) : string =
  "Loop: {head: " ^ (block_name l.loop_head) ^
  "; body: " ^ (pr_list block_name l.loop_body) ^
  "; exit: " ^ (pr_list block_name l.loop_exit) ^ "}"


let pr_loops (ls: loop list) : string =
  pr_items pr_loop ls


let pr_block (blk: block) : string =
  let blkname = block_name blk in
  let sinstrs = blk |> map_instrs ~f:(hpr_indent 2 pr_instr) |>
                String.concat ~sep:"\n" in
  " " ^ blkname ^ ":\n" ^
  (String.replace_if_empty sinstrs ~replacer:"{Empty block}")


let pr_func (f: func) : string =
  let fname = "Function: " ^ (pr_type (func_return_type f)) ^ " " ^
              (func_name f) ^
              "(" ^ (pr_args pr_typed_param (func_params f)) ^ ")" in
  let sblks = f |> map_blocks ~f:pr_block |>
              String.concat ~sep:"\n\n" |>
              String.replace_if_empty ~replacer:"{Empty function}" in
  fname ^ "\n" ^ sblks


let pr_module (m: llmodule) : string =
  LL.string_of_llmodule m


let pr_program (prog: program) : string =
  let sglobals = prog.prog_globals |>
                 List.map ~f:(hpr_indent 2 (pr_global ~detailed:true)) |>
                 String.concat ~sep:"\n" |>
                 String.prefix_if_not_empty ~prefix:"Globals:\n" in
  let sstructs = prog.prog_struct_types |>
                 List.map ~f:(fun t -> "  " ^ (pr_type t)) |>
                 String.concat ~sep:"\n" |>
                 String.prefix_if_not_empty ~prefix:"Struct types:\n" in
  let funcs = prog.prog_init_funcs @ prog.prog_user_funcs in
  let sfuncs = funcs |> List.map ~f:pr_func |>
               String.concat ~sep:"\n\n" in
  (String.suffix_if_not_empty sglobals ~suffix:"\n\n") ^
  (String.suffix_if_not_empty sstructs ~suffix:"\n\n") ^
  sfuncs


let pr_caller_info (prog: program) : string =
  Hashtbl.fold
    ~f:(fun ~key:func ~data:callers acc ->
         let fname = func_name func in
         let caller_names = callers |> List.map ~f:func_name |>
                            String.concat ~sep:", " in
         acc ^ "\n  " ^ fname ^ " <-- [" ^ caller_names ^ "]")
    ~init:"Caller graph:" prog.prog_func_callers


let pr_callee_info (prog: program) : string =
  Hashtbl.fold
    ~f:(fun ~key:func ~data:callees acc ->
         let fname = func_name func in
         let callee_names = callees |> List.map ~f:func_name |>
                            String.concat ~sep:", " in
         acc ^ "\n  " ^ fname ^ " --> [" ^ callee_names ^ "]")
    ~init:"Callee graph:" prog.prog_func_callees


let print_program_analysis_info prog =
  let callees_info =
    "====================================\n" ^
    "* Information of function callees:\n" ^
    Hashtbl.fold
      ~f:(fun ~key:f ~data:callees acc ->
           if List.is_empty callees then acc
           else acc ^ "\n - " ^ (func_name f) ^ ":" ^
                (pr_items ~bullet:"    ->" func_name callees))
      ~init:"" prog.prog_func_callees in
  let _ = debug callees_info in
  let callers_info =
    "====================================\n" ^
    "* Information of function callers:\n" ^
    Hashtbl.fold
      ~f:(fun ~key:f ~data:callers acc ->
           if List.is_empty callers then acc
           else acc ^ "\n - " ^ (func_name f) ^ ":" ^
                (pr_items ~bullet:"    <-" func_name callers))
      ~init:"" prog.prog_func_callers in
  debug callers_info
