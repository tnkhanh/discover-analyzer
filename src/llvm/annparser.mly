%token OPEN_COM
%token CLOSE_COM
%token OPEN
%token CLOSE
%token COLON
%token BUG
%token SAFE
%token EOF
%token <string> ANNCONT

%start <Ann.value option> prog
%%

prog:
| v = value { Some v }
| EOF       { None   } ;

value:
| OPEN_COM;OPEN;BUG;COLON; k=ANNCONT ; CLOSE_COM { `Bug_start k  }
| OPEN_COM;OPEN;SAFE;COLON; k=ANNCONT ; CLOSE_COM { `Safe_start k  }
| OPEN_COM;COLON;BUG;CLOSE; CLOSE_COM { `Bug_end }
| OPEN_COM;COLON;SAFE;CLOSE; CLOSE_COM { `Safe_end }
