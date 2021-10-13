{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let not_annot = [^'*']*
let ann_content = [^'*']*

rule read =
  parse
  | white    { read lexbuf }
  | not_annot { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "/*" { OPEN_COM }
  | "*/" { CLOSE_COM }
  | ':'      { COLON }
  | '{'      { OPEN }
  | '}'      { CLOSE }
  | "Bug"      { BUG }
  | "Safe"     { SAFE }
  | ann_content { Lexing.lexeme lexbuf }
  | eof      { EOF }
