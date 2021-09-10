(********************************************************************
 * Author: Ta Quang Trung
 * Date: 2020
 *
 * Copyright (c) 2020-2021 Singapore Blockchain Innovation Programme.
 * All rights reserved.
 ********************************************************************)

{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1 }

let raise_syntax_error msg lexbuf =
  raise (SyntaxError (msg ^ ": " ^ (Lexing.lexeme lexbuf)))
}

let int = ['0'-'9']['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '*']*

let string = [^ '"' '\\']+

rule read = parse
  | white                { read lexbuf }
  | newline              { next_line lexbuf; read lexbuf }
  (* | '"'               { read_string (Buffer.create 20) lexbuf } *)
  | "//"                 { skip_line_comment lexbuf }
  | "/*"                 { skip_block_comment lexbuf }
  | "true"               { TRUE }
  | "false"              { FALSE }
  | "emp"                { EMP }
  | "view"               { VIEW }
  | "data"               { DATA }
  | "rel"                { REL }
  | "func"               { FUNC }
  | "array"              { ARRAY }
  | "CheckSat"           { CHECK_SAT }
  | "ProveEntails"       { PROVE_ENTAILS }
  | "InferFrame"         { INFER_FRAME }
  | "forall"             { FORALL }
  | "exists"             { EXISTS }
  | "requires"           { REQUIRES }
  | "ensures"            { ENSURES }
  | "void"               { VOID }
  | "byte"               { BYTE }
  | "int"                { INT }
  | "bool"               { BOOL }
  | "\\/"                { VEE }
  | "->"                 { RARROW }
  | "|-"                 { ENTAIL }
  | ":="                 { EQCOLON }
  | ">="                 { GE }
  | "<="                 { LE }
  | "!="                 { NE }
  | "\\*"                { MULT }
  | "*+"                 { UPDATE }
  | "*-"                 { SEPTRACT }
  | '>'                  { GT }
  | '<'                  { LT }
  | '='                  { EQ }
  | '!'                  { NOT }
  | '&'                  { AND }
  | '|'                  { OR }
  | '+'                  { PLUS }
  | '-'                  { MINUS }
  | '*'                  { STAR }
  | "--*"                { WAND }
  | '/'                  { DIV }
  | '('                  { LPAREN }
  | ')'                  { RPAREN }
  | '{'                  { LBRACE }
  | '}'                  { RBRACE }
  | '@'                  { AT }
  | ':'                  { COLON }
  | ','                  { COMMA }
  | ';'                  { SEMICOLON }
  | '.'                  { DOT }
  | '?'                  { QUESTION }
  | '#'                  { HASH }
  | id                   { ID (Lexing.lexeme lexbuf) }
  | int                  { INT_LIT (int_of_string (Lexing.lexeme lexbuf)) }
  (* | float             { FLOAT_LIT (float_of_string (Lexing.lexeme lexbuf)) } *)
  | _                    { raise_syntax_error "Unexpected char:" lexbuf }
  | eof                  { EOF }

and skip_block_comment = parse
  | "*/"      { read lexbuf }
  | eof       { EOF }
  | _         { skip_block_comment lexbuf }

and skip_line_comment = parse
  | newline   { read lexbuf }
  | eof       { EOF }
  | _         { skip_line_comment lexbuf }
