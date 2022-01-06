(********************************************************************
 * This file is part of the source code analyzer Discover.
 *
 * Copyright (c) 2020-2022 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

(*******************************************************************
 ** Header
 *******************************************************************)

%{
open Z3ir
%}


(*******************************************************************
 ** Token
 *******************************************************************)

%token <int> INT_LIT
%token <float> FLOAT_LIT
%token <string> ID
%token <string> STRING_LIT
%token LPAREN RPAREN
%token MODEL DEFFUN INT REAL TOINT MINUS MULT DIV EOF
%token SAT UNSAT UNK ERR

%start <Z3ir.z3_res> output
%%

(*******************************************************************
 ** Production rules
 *******************************************************************)

output:
  | EOF                                 { Unk }
  | UNSAT; output                       { Unsat }
  | SAT; output                         { Sat [] }
  | SAT; m=model; output                { Sat m }
  | UNK; output                         { Unk }
  | msg=error_message; output           { Error msg }

error_message:
  | LPAREN; ERR; msg=STRING_LIT; RPAREN { msg }

model:
  | LPAREN; MODEL; m=sol_list; RPAREN   { m }

sol_list:
	| s=list(sol)                         { s }

(* TODO : need to parse function as well *)
sol:
  | LPAREN; DEFFUN; id=ID; LPAREN; RPAREN; typ; v=value; RPAREN { (id, v) }

typ:
  | INT                             {}
  | REAL                            {}

value:
  | v=prim_val                      { v }
  | TOINT; v=prim_val               { v }
  | MULT; v1=value; v2=value        { z3_val_mult v1 v2}
  | LPAREN; v=value; RPAREN         { v }

prim_val:
  | v=int_val                       { v }
  | v=frac_val                      { v }
  | LPAREN; v=prim_val; RPAREN      { v }
  | MINUS; v=prim_val               { z3_val_neg v }

int_val:
  | v=INT_LIT                       { Int v }

frac_val:
  | DIV; v1=FLOAT_LIT; v2=FLOAT_LIT { Frac (v1, v2) }
  | v=FLOAT_LIT                     { Frac (v, 1.0) }
