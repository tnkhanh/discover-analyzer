(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(*******************************************************************
 ** Header
 *******************************************************************)

%{
open Globals
open Slast

let get_loc l = mk_position_lexing (fst l) (snd l)

%}

(*******************************************************************
 ** Token
 *******************************************************************)

%token <int> INT_LIT
(* %token <string> STRING_LIT *)
%token <string> ID
%token BOOL INT BYTE VOID
%token TRUE FALSE EMP
%token FUNC REL VIEW DATA ARRAY
%token PLUS MINUS MULT DIV
%token STAR WAND UPDATE SEPTRACT
%token LPAREN RPAREN LBRACE RBRACE
%token EQ NE LT LE GT GE
%token AND OR NOT
%token FORALL EXISTS
%token ENTAIL VEE
%token RARROW
%token EQCOLON AT
%token COLON COMMA SEMICOLON DOT QUESTION HASH
%token CHECK_SAT PROVE_ENTAILS INFER_FRAME
%token REQUIRES ENSURES
%token EOF

%left PLUS MINUS
%left MULT DIV
%nonassoc NOT
%left AND OR

(* the earlier token has higher reduce priority *)
%on_error_reduce exp


%start <Slast.program> program
%%

(*******************************************************************
 ** Rules
 *******************************************************************)

program :
  | EOF
      { mk_program_empty () }
  | FUNC; f = func_defn; SEMICOLON; pg = program
      { {pg with prog_func_defns = pg.prog_func_defns @ [f]} }
  | DATA; d = data_defn; SEMICOLON; pg = program
      { {pg with prog_data_defns = pg.prog_data_defns @ [d]} }
  | VIEW; v = view_defn; SEMICOLON; pg = program
      { {pg with prog_pred_defns = pg.prog_pred_defns @ [v]} }
  | REL; r = reln_defn; SEMICOLON; pg = program
      { {pg with prog_pred_defns = pg.prog_pred_defns @ [r]} }
  | p = proc_defn; pg = program
      { {pg with prog_proc_defns = pg.prog_proc_defns @ [p]} }
  | c = command; pg = program
      { {pg with prog_commands = pg.prog_commands @ [c]} }

command :
  | CHECK_SAT; COLON; f = formula; SEMICOLON
      { CheckSat (f, get_loc $loc) }
  | PROVE_ENTAILS; COLON; ents = entailment_list_hash; SEMICOLON
      { ProveEntails (ents, get_loc $loc) }
  | PROVE_ENTAILS; COLON; ents = entailment_list_comma; SEMICOLON
      { ProveEntails (ents, get_loc $loc) }
  | INFER_FRAME; COLON; ent = entailment; SEMICOLON
      { InferFrame (ent, get_loc $loc) }

data_defn :
  | n = ID; LBRACE; fs = field_decl_list; RBRACE
      { mk_data_defn (TStruct n) fs (get_loc $loc) }

func_defn :
  | n = ID; LPAREN; p = var_list; RPAREN ; EQCOLON; b = func_body
      { mk_func_defn n p b (get_loc $loc) }

func_body :
  | QUESTION
      { None }
  | e = exp
      { Some e }

view_defn :
  | n = ID; LPAREN; p = var_list; RPAREN ; EQCOLON; b = view_body
      { mk_pred_defn n p PtView b (get_loc $loc) }

view_body :
  | QUESTION
      { [] }
  | fs = separated_nonempty_list(VEE, formula)
      { fs }

reln_defn :
  | n = ID; LPAREN; p = var_list; RPAREN ; EQCOLON; b = reln_body
      { mk_pred_defn n p PtReln b (get_loc $loc) }

reln_body :
  | QUESTION
      { [] }
  | f = formula
      { [f] }

proc_defn :
  | t = typ; n = ID; LPAREN; ps = param_decl_list; RPAREN;
    specs = nonempty_list(proc_specs)
      { mk_proc_defn n ps t specs (get_loc $loc) }

proc_specs :
  | REQUIRES; pre = formula; ENSURES; post = formula; SEMICOLON;
      { mk_proc_specs pre post }

entailment :
  | b = formula; ENTAIL; h = formula
      { mk_entailment b h }

entailment_list_comma:
  | ents = separated_nonempty_list(COMMA, entailment)
      { ents }

entailment_id :
  | HASH; id = INT_LIT; DOT; b = formula; ENTAIL; h = formula
      { mk_entailment ~id:id b h }
  | HASH; b = formula; ENTAIL; h = formula
      { mk_entailment ~id:(-1) b h }

entailment_list_hash:
  | ents = nonempty_list(entailment_id)
      { ents }

formula :
  | b = bool
      { Bool (b, get_loc $loc) }
  | v = var
      { BVar (v, get_loc $loc) }
  | EMP
      { Emp (get_loc $loc) }
  | e1 = exp; EQ; e2 = exp
      { BinRel (Eq, e1, e2, get_loc $loc) }
  | e1 = exp; NE; e2 = exp
      { BinRel (Ne, e1, e2, get_loc $loc) }
  | e1 = exp; LT; e2 = exp
      { BinRel (Lt, e1, e2, get_loc $loc) }
  | e1 = exp; LE; e2 = exp
      { BinRel (Le, e1, e2, get_loc $loc) }
  | e1 = exp; GT; e2 = exp
      { BinRel (Gt, e1, e2, get_loc $loc) }
  | e1 = exp; GE; e2 = exp
      { BinRel (Ge, e1, e2, get_loc $loc) }
  | r = exp; addr = option(address);
    RARROW; n = ID; LBRACE; es = exp_list; RBRACE
      { Data (r, (TStruct n), es, addr, get_loc $loc) }
  | n = ID; LPAREN; es = exp_list; RPAREN
      { Pred (n, es, get_loc $loc)}
  | ARRAY; LPAREN; r = exp; COMMA; s = exp; COMMA; t = typ; RPAREN
      { Array (r, s, t, get_loc $loc) }
  | NOT; f = formula
      { Neg f }
  | e = exp; EQ; LPAREN; f = formula; RPAREN
      { BEq (e, f) }
  | f1 = formula; AND; f2 = formula
      { Conj (f1, f2) }
  | f1 = formula; OR; f2 = formula
      { Disj (f1, f2) }
  | f1 = formula; STAR; f2 = formula
      { Star (f1, f2) }
  | f1 = formula; WAND; f2 = formula
      { Wand (f1, f2) }
  | f1 = formula; UPDATE; f2 = formula
      { Update (f1, f2) }
  | f1 = formula; SEPTRACT; f2 = formula
      { Septract (f1, f2) }
  | LPAREN; EXISTS; vs = var_list; DOT; f = formula; RPAREN
      { Exists (vs, f) }
  | LPAREN; FORALL; vs = var_list; DOT; f = formula; RPAREN
      { Forall (vs, f) }
  | LPAREN; f = formula; RPAREN
      { f }

address :
  | AT; LPAREN; base = exp; COMMA; elem = exp; COMMA; field = exp; RPAREN
      { {addr_base = base; addr_elem = elem; addr_field = field;} }

bool :
  | TRUE
      { true }
  | FALSE
      { false }

exp_list :
  | es = separated_list(COMMA, exp)
      { es }

exp :
  | i = INT_LIT
      { Int (i, get_loc $loc) }
  | v = var
      { Var (v, get_loc $loc) }
  | e1 = exp; PLUS; e2 = exp
      { BinExp (Add, e1, e2, get_loc $loc) }
  | e1 = exp; MINUS; e2 = exp
      { BinExp (Sub, e1, e2, get_loc $loc) }
  | e1 = exp; MULT; e2 = exp
      { BinExp (Mul, e1, e2, get_loc $loc) }
  | e1 = exp; DIV; e2 = exp
      { BinExp (Div, e1, e2, get_loc $loc) }
  | n = ID; LPAREN; es = exp_list; RPAREN
      { Func (n, es, TUnk, (get_loc $loc)) }
  | LPAREN; e = exp; RPAREN
      { e }

var_list :
  | vs = separated_list(COMMA, var)
      { vs }

var :
  | n = ID
      { mk_var n TUnk }
  | n = ID; COLON; t = typ
      { mk_var n t }

field_decl_list :
  | fs = list(field_decl);
      { fs }

field_decl :
  | t = typ; n = ID; SEMICOLON
      { (t, n) }

param_decl_list :
  | ps = separated_list(COMMA, param_decl);
      { ps }

param_decl :
  | t = typ; n = ID
      { mk_var n t }

typ :
  | VOID
      { TVoid }
  | BOOL
      { TBool }
  | BYTE
      { TInt 8 }
  | INT
      { TInt 32 }
  | t = typ; STAR
      { TPointer t }
  | n = ID
      { TStruct n }
