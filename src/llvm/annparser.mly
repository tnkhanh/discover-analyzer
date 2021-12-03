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
  | WORD
  | SLASH
  | ASTER
  | OBRAC
  | CBRAC
  | COLON
  | ATYPE
  | COMMA { Skip }
  | a = ann_begin { a }
  | a = ann_end { a };

bugs:
  value = separated_list(COMMA, bug) { value };

bug:
    | MEMORY_LEAK { MemoryLeak }
    | NULL_POINTER_DEREF { NullPointerDeref }
    | BUFFER_OVERFLOW { BufferOverflow }
    | INTEGER_OVERFLOW { IntegerOverflow }
    | INTEGER_UNDERFLOW { IntegerUnderflow }
    | DIVISION_BY_ZERO { DivisionByZero }
    | w = WORD { Ann.NewType w };

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
