%{
open Lexing
open Parsing
open Parsed

%}

%token <string> IDENT
%token AX EX AG EG AF EF A E U R TRUE FALSE
%token LP RP LB RB LCB RCB CASE ESAC
%token INIT NEXT 
%token NOT OR AND IMP EQUV EQ
%token COMMA COLON SEMI EQDEF EOF
%token MODULE PROCESS BOOLEAN VAR INIT SPEC ASSIGN END


/* Precedence */
%right OR
%right AND
%right IMP
%right EQUV
%nonassoc NOT
%nonassoc AX EX AG EG AF EF A E TRUE FALSE
%right U R
%nonassoc IDENT

%type <Parsed.file>file
%start file
%%

file:
|list_decl EOF {$1}
|EOF {[]}
;

list_decl:
| decl {[$1]}
| decl list_decl {$1 :: $2}
;

decl:
| MODULE ident VAR var_list ASSIGN assign_list SPEC spec
    {Module1($2,$4, $6, $8)}
| MODULE ident LP idents RP VAR var_list ASSIGN assign_list
	{Module2($2,$4,$7, $9)}
;

var_list:
| /*empty list*/ {[]}
| var SEMI var_list {$1::$3}
;

var:
| ident COLON BOOLEAN {Boolean($1)}
| ident COLON LCB idents RCB {Elem($1,$4)}
| ident COLON PROCESS ident LP idents RP {Proc($1,$4,$6)}
;

assign_list:
| /*empty list*/ {[]}
| assign SEMI assign_list {$1::$3}
;

assign:
| INIT LP ident RP EQDEF exp {Init($3, $6)}
| NEXT LP ident RP EQDEF exp {Next($3,$6)}
| NEXT LP ident RP EQDEF CASE case_assigns ESAC {CNext($3, $7)}
;

case_assigns:
|/* empty cases*/ {[]}
| exp COLON exp SEMI case_assigns {($1, $3)::$5}
;

spec:
| ident EQ exp {Eq($1, $3)}
| NOT spec {Neg($2)}
| LP spec RP {$2}
| spec AND spec {And($1, $3)}
| spec OR spec {Or($1, $3)}
| AX LP spec RP {AX($3)}
| EX LP spec RP {EX($3)}
| AF LP spec RP {AF($3)}
| EF LP spec RP {EF($3)}
| AG LP spec RP {AG($3)}
| EG LP spec RP {EG($3)}
| A LB spec U spec RB {AU($3,$5)}
| E LB spec U spec RB {EU($3,$5)}
| A LB spec R spec RB {AR($3,$5)}
| E LB spec R spec RB {ER($3,$5)}
;

exp:
| TRUE {True}
| FALSE {False}
| NOT exp { Neg($2)}
| exp AND exp {And($1, $3)}
| exp OR exp {Or($1, $3)}
| exp EQ exp {Eq($1, $3)}
| ident {Var($1)}
;


idents:
|/*empty idents*/ {[]}
| ident {[$1]}
| ident COMMA idents {$1::$3}
;

ident:
IDENT {$1}
;
