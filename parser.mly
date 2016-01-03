%{
open Parsing
open Parsed

%}

%token <string> IDENT
%token AX EX AG EG AF EF A E U R TRUE FALSE
%token LP RP LB RB LCB RCB
%token EQ NOTEQ
%token NOT OR AND
%token COMMA COLON SEMI EQDEF 
%token MODULE PROCESS BOOLEAN VAR INIT SPEC ASSIGN

/* Precedence */
%right OR
%right AND
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
decl {[$1]}
| decl list_decl {$1 :: $2}
;

decl:
| MODULE  ident varTypeList assignList spec END 
    {Module1($2,$3,$4,$5)}
| MODULE ident LP vars RP varTypeList assignList END
	{Module($2,$4,$6,$7)}
;

varTypeList:
| /*empty list*/ {[]}
| varType varTypeList {$1 :: $2}
;

varType:
| ident COLON BOOLEAN SEMI {Boolean($1)}
| ident COLON LCB idents RCB {Elem($1,$4)}
| ident COLON PROCESS ident LP vars RP {Process($1,$4,$6)}
;

assignList:
| INIT LP ident RP EQDEF 


ident:
IDENT {$1}
;


var_list:
| /*empty list*/ {[]}
| var var_list {$1::$2}
;

assign_list:
| /*no assigns*/ {[]}
| assgin assign_list {$1::$2}
;

spec:
