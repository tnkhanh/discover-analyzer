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
%}

(*******************************************************************
 ** Token
 *******************************************************************)

%token <int> ANNSTART
%token COLON
%token MEMORY_LEAK
%token NULL_POINTER_DEREF
%token BUFFER_OVERFLOW
%token INTEGER_OVERFLOW
%token INTEGER_UNDERFLOW
%token DIVISION_BY_ZERO
%token <string> WORD
%token EOF
%token COMMA

%start <Line_annot.program> prog
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
        | None -> []
        | Some a -> [a] }
  | ts = toks; t = tok
      { match t with
        | None -> ts
        | Some a -> a::ts };

tok:
  | WORD
  | COLON
  | COMMA { None }
  | a = ann { a }

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

ann:
  | l = ANNSTART; w = WORD; COLON; b = bugs 
      { Some (l, w, b)};
