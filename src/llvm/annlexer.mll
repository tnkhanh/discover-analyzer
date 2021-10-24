{
open Lexing
open Annparser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = pos.pos_cnum;
               pos_lnum = pos.pos_lnum + 1
    }
}

(*let int = '-'? ['0'-'9'] ['0'-'9']*

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp? *)

(*let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let word = _*
let ann_content = _* 

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "/*" { OPEN_COM }
  | "*/" { CLOSE_COM }
  | ':'      { COLON }
  | '{'      { OPEN }
  | '}'      { CLOSE }
  | "Bug"      { BUG }
  | "Safe"     { SAFE }
  | ann_content { ANNCONT (Lexing.lexeme lexbuf) } 
  | eof | word { WORD (Lexing.lexeme lexbuf) } *)

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let word = ['a'-'z' 'A'-'Z' '_' '0'-'9' '[' ']' '.' ',' '"' '#' '%' '(' ')'
'<' '>' '=' '+' '-' '\\' ';' '&' '`' '\''] +

(*rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "Bug" | "Safe" { ATYPE (Lexing.lexeme lexbuf) }
  | '/' { SLASH }
  | '*' { ASTER }
  | '{' { OBRAC }
  | '}' { CBRAC }
  | ':' { COLON }
  | word       { WORD (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) } *)

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "/*@" { ANNSTART }
  | "Bug" | "Safe" { ATYPE (Lexing.lexeme lexbuf) }
  | '/' { SLASH }
  | '*' { ASTER }
  | '{' { OBRAC }
  | '}' { CBRAC }
  | ':' { COLON }
  | word       { WORD (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
