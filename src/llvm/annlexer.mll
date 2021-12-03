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

let curr_pos lexbuf =
  let pos = lexbuf.lex_curr_p in (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)
}

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let word = [^ ' ' '\t' '\n' '\r' '/' '*' '{' '}' ':' ',' ] +

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "/*@" { ANNSTART (curr_pos lexbuf) }
  | "Bug" | "Safe" { ATYPE (Lexing.lexeme lexbuf) }
  | "MemoryLeak" { MEMORY_LEAK }
  | "NullPointerDeref" { NULL_POINTER_DEREF }
  | "BufferOverflow" { BUFFER_OVERFLOW }
  | "IntegerOverflow" { INTEGER_OVERFLOW }
  | "IntegerUnderflow" { INTEGER_UNDERFLOW }
  | "DivisionByZero" { DIVISION_BY_ZERO }
  | '/' { SLASH ( curr_pos lexbuf ) }
  | '*' { ASTER }
  | '{' { OBRAC }
  | '}' { CBRAC }
  | ':' { COLON }
  | ',' { COMMA }
  | word { WORD (Lexing.lexeme lexbuf) }
  | eof { EOF }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
