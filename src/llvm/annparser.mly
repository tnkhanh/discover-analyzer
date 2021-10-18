(*%token OPEN_COM
%token CLOSE_COM
%token OPEN
%token CLOSE
%token COLON
%token BUG
%token SAFE
%token EOF
%token <string> ANNCONT
%token <string> WORD

%start <Ann.prog> program
%%

program:
| {[]}
| p = program; m = mark {m::p};

mark: *)
(*| OPEN_COM;OPEN;BUG;COLON; k=ANNCONT ; CLOSE_COM { `Bug_start k  }
| OPEN_COM;OPEN;SAFE;COLON; k=ANNCONT ; CLOSE_COM { `Safe_start k  }
| OPEN_COM;COLON;BUG;CLOSE; CLOSE_COM { `Bug_end }
| OPEN_COM;COLON;SAFE;CLOSE; CLOSE_COM { `Safe_end } 
| WORD | OPEN_COM | CLOSE_COM | OPEN | CLOSE | COLON | BUG | SAFE | EOF { `Word }
| w=WORD { `Word w }; *)

%token SLASH
%token ASTER
%token OBRAC
%token CBRAC
%token COLON
%token <string> ATYPE
%token <string> WORD
%token EOF
%right DUMMY
%right SLASH ASTER OBRAC CBRAC COLON ATYPE WORD EOF

%start <string list> prog
%%

prog:
| EOF (* empty *) {[]}
| ss = supertoks; EOF {ss};

supertoks:
| s = supertok {[s]}
| ss = supertoks; s = supertok { s::ss };

supertok:
  | t=tok {t}
  | o=open_mark {o}

open_mark:
  | SLASH;ASTER;OBRAC;a=ATYPE;COLON;w=WORD;ASTER;SLASH {a^": "^w }

tok:
  | w = WORD { "WORD "^w } %prec DUMMY
  | SLASH { "SLASH" } %prec DUMMY
  | ASTER { "ASTER" } %prec DUMMY
  | OBRAC { "OBRAC" } %prec DUMMY
  | CBRAC { "CBRAC" } %prec DUMMY
  | COLON { ":" }  %prec DUMMY
  | a = ATYPE { a } %prec DUMMY
