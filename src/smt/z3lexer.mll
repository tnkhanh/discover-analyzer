{
open Z3parser


exception SyntaxError of string
}

let alpha = ['a'-'z' 'A'-'Z' '_' '!']
let digit = ['0'-'9']
let dot = '.'
let id = alpha(alpha|digit)*
let intnum = digit+
let floatnum = (digit+)dot(digit*) | (digit*)dot(digit+)
let whitespace = [' ' '\t' '\n' '\r']+

rule tokenizer =
  parse
  | "model"            { MODEL }
  | "sat"              { SAT }
  | "unsat"            { UNSAT }
  | "unknown"          { UNK }
  | "error"            { ERR }
  | '('                { LPAREN }
  | ')'                { RPAREN }
  | "define-fun"       { DEFFUN }
  | "to_int"           { TOINT }
  | "Int"              { INT }
  | "Real"             { REAL }
  | '-'                { MINUS }
  | '/'                { DIV }
  | '*'                { MULT }
  | "Z3EOF"            { EOF }
	| eof                { EOF }
  | intnum as numstr   { INT_LIT (int_of_string numstr) }
  | floatnum as numstr { FLOAT_LIT (float_of_string numstr) }
  | id as idstr        { ID idstr }
  | '"'                { read_str (Buffer.create 20) lexbuf }
  | whitespace         { tokenizer lexbuf }

and read_str buf =
  parse
  | '"'       { STRING_LIT (Buffer.contents buf) }
  | '\\' '/'  { Buffer.add_char buf '/';    read_str buf lexbuf }
  | '\\' '\\' { Buffer.add_char buf '\\';   read_str buf lexbuf }
  | '\\' 'b'  { Buffer.add_char buf '\b';   read_str buf lexbuf }
  | '\\' 'f'  { Buffer.add_char buf '\012'; read_str buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n';   read_str buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r';   read_str buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t';   read_str buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_str buf lexbuf}
  | eof { raise (SyntaxError ("String is not terminated")) }
  | _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
