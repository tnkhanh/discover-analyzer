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

(*%token SLASH
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
  | a = ATYPE { a } %prec DUMMY *)

%token <int * int> ANNSTART
%token <int * int> SLASH
%token ASTER
%token OBRAC
%token CBRAC
%token COLON
%token <string> ATYPE
%token <string> WORD
%token EOF
%token COMMA
(*%right DUMMY
%right SLASH ASTER OBRAC CBRAC COLON ATYPE WORD EOF *)

%start <Ann.program> prog
%%

prog:
| EOF (* empty *) {[]}
| ts = toks; EOF {ts};

toks:
  | t = tok { 
    match t with 
    | Ann.Skip -> [] 
    | _ as m -> [m]
    }
  | ts = toks; t = tok {
    match t with 
    | Ann.Skip -> ts 
    | _ as m -> m::ts
    };

tok:
  | WORD { Skip  }
  | SLASH { Skip }
  | ASTER { Skip }
  | OBRAC { Skip }
  | CBRAC { Skip }
  | COLON { Skip }
  | ATYPE { Skip }
  | COMMA { Skip }
  | a = ann_begin { a }
  | a = ann_end { a };

bugs:
  value = separated_list(COMMA, bug) { value };

bug:
  w = WORD {
    match w with
    | "MemoryLeak" -> Ann.MemoryLeak
    | "NullPointerDeref" -> NullPointerDeref
    | "BufferOverflow" -> BufferOverflow
    | "IntegerOverflow" -> IntegerOverflow
    | "IntegerUnderflow" -> IntegerUnderflow
    | _ as s -> NewType s
  }

ann_begin:
  | ANNSTART; OBRAC; a = ATYPE; COLON; b = bugs; ASTER; p = SLASH 
    {if a = "Bug" then Bug_start (p, b) 
     else
       Safe_start (p, b)
    };

ann_end:
  | p = ANNSTART;COLON; a = ATYPE; CBRAC; ASTER; SLASH 
    {
      if a = "Bug" then Bug_end p
      else
        Safe_end p
    };
