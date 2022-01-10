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
  open Dcore
%}

(*******************************************************************
 ** Token
 *******************************************************************)

%token <int * int> ANNSTART
%token <int * int> SLASH
%token ASTER
%token OBRAC
%token CBRAC
%token COLON
%token MEMORY_LEAK
%token NULL_POINTER_DEREF
%token BUFFER_OVERFLOW
%token INTEGER_OVERFLOW
%token INTEGER_UNDERFLOW
%token DIVISION_BY_ZERO
%token <string> ATYPE
%token <string> WORD
%token EOF
%token COMMA

%start <Llannot.program> prog
%%

(*******************************************************************
 ** Rules
 *******************************************************************)

prog:
  | EOF (* empty *)
      {[]}
  | ts = toks; EOF
      {ts};

toks:
  | t = tok
      { match t with
        | Llannot.Skip -> []
        | _ as m -> [m] }
  | ts = toks; t = tok
      { match t with
        | Llannot.Skip -> ts
        | _ as m -> m::ts };

tok:
  | WORD
  | SLASH
  | ASTER
  | OBRAC
  | CBRAC
  | COLON
  | ATYPE
  | COMMA { Llannot.Skip }
  | a = ann_begin { a }
  | a = ann_end { a };

bugs:
  value = separated_list(COMMA, bug) { value };

bug:
  | MEMORY_LEAK
      { Bug.mk_bug_type_memory_leak () }
  | NULL_POINTER_DEREF
      { Bug.mk_bug_type_null_pointer_deref () }
  | BUFFER_OVERFLOW
      { Bug.mk_bug_type_memory_leak () }
  | INTEGER_OVERFLOW
      { Bug.mk_bug_type_integer_overflow () }
  | INTEGER_UNDERFLOW
      { Bug.mk_bug_type_integer_underflow () }
  | DIVISION_BY_ZERO
      { Bug.mk_bug_type_division_by_zero () }

ann_begin:
  | ANNSTART; OBRAC; a = ATYPE; COLON; b = bugs; ASTER; p = SLASH
      { let line, col = p in
        let pos = Llannot.mk_annot_position line col in
        if String.equal a "Bug" then Llannot.Bug_start (pos, b)
        else Safe_start (pos, b) };

ann_end:
  | p = ANNSTART; COLON; a = ATYPE; CBRAC; ASTER; SLASH
      { let line, col = p in
        let pos = Llannot.mk_annot_position line col in
        if String.equal a "Bug" then Llannot.Bug_end pos
        else Safe_end pos };
