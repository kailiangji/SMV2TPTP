%{
open Lexing
open Parsing
open Parsed

let rec seprate_vars_list_helper (bvar, nbvar, nbconst, proc) l =
  match l with
  | [] -> accu
  | Bvar(var) :: tl -> 
     seprate_vars_list_helper (BVar(var)::bvar, nbvar, proc) tl
  | NBvar(var,conlst) :: tl ->
     seprate_vars_list_helper (bvar, NBVar(var)::nbvar,  proc) tl
  | Process _,_,_ as h :: tl ->
     seprate_vars_list_helper (bvar, nbvar, h::proc) tl

let seprate_vars_list = seprate_vars_list_helper ([],[],[],[])

let rec get_nbconst nbvar nbconst_l =
  match nbconst_l with
  | [] -> []
  | h :: tl -> NBConst(h,nbvar) :: (get_nbconst nbvar tl)

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
| MODULE ident VAR var_list ASSIGN assign_list SPEC spec END 
    {Module1($2,$4,$6,$8)}
| MODULE ident LP idents RP VAR var_list ASSIGN assign_list END
	{Module2($2,$4,$7,$9)}
;

var_list:
| /*empty list*/ {[]}
| var var_list {$1::$2}
;

var:
| ident COLON BOOLEAN SEMI {BVar($1)}
| ident COLON LCB idents RCB SEMI {NBVar($1,$4)}
| ident COLON PROCESS ident LP idents RP SEMI{Process($1,$4,$6)}
;

assign_list:
| /* empty assigns*/ {[]}
| INIT LP ident RP EQDEF expr SEMI assign_list {Init($3,$6)::$8}
| NEXT LP ident RP EQDEF expr SEMI assign_list {Next($3,$6)::$8}
| NEXT LP ident RP EQDEF CASE c_assign ESAC SEMI assign_list 
    {NextCase($3,$7)::$10}
;

expr:
| lexpr {$1}
| ident {Elem2($1)}
;

lexpr:
| NOT lexpr {Neg($2)}
| TRUE {True}
| FALSE {False}
| lexpr infix lexpr {Infix($1,$2,$3)}
| ident { if List.mem }
;

infix:
| AND {Land}
| OR {Lor}
| IMP {Limp}
| EQUV {Leqv}
;

spec:
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
| spec OR spec {Or($1,$3)}
| spec AND spec {And($1,$3)}
| spec IMP spec {Imp($1,$3)}
| spec EQUV spec {Equv($1,$3)}
| ident EQ expr {Pprop($1, $3)}
;



idents:
|/*empty idents*/ {[]}
| ident {[$1]}
| ident COMMA idents {$1::$3}
;

ident:
IDENT {$1}
;
