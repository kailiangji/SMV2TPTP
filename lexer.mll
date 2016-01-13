{
  open Lexing
  open Parser
 
  let keywords = Hashtbl.create 30
  let () =
    List.iter
      (fun (x,y) -> Hashtbl.add keywords x y)
      [ "MODULE", MODULE;
	"process", PROCESS;
	"boolean", BOOLEAN;
	"VAR", VAR;
	"init", INIT;
	"next", NEXT;
	"SPEC", SPEC;
	"ASSIGN", ASSIGN;
	"case", CASE;
	"esac", ESAC;
	"AX", AX;
	"EX", EX;
        "AG", AG;
	"EG", EG;
	"AF", AF;
	"EF", EF;
	"A", A;
	"E", E;
	"U", U;
	"R", R;
	"TRUE", TRUE;
	"FALSE", FALSE
      ]

 let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
     {pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }

 let string_buf = Buffer.create 1024
   
 exception Lexical_errof of string
     
}

let space = [' ' '\t' '\r']
let alpha = ['a'-'z' 'A'-'Z']
let letter = alpha | '_'
let digit = ['0'-'9']
let ident = (letter) (letter | digit)*

rule token = parse
| '\n'
        {newline lexbuf; token lexbuf}
| space+
	{token lexbuf}
| ident as id (* identifiers *)
	    { try
		Hashtbl.find keywords id
	      with Not_found ->
		IDENT id
	    }
| "--"	{comment lexbuf; token lexbuf}
| '(' {LP}
| ')' {RP}
| '[' {LB}
| ']' {RB}
| '{' {LCB}
| '}' {RCB}
| ":=" {EQDEF}
| '=' {EQ}
| '!' {NOT}
| '|' {OR}
| '&' {AND}
| "=>" {IMP}
| "<=>" {EQUV}
| ',' {COMMA}
| ':' {COLON}
| ';' {SEMI}
| eof {EOF}    
and comment = parse
    | '\n'
	{ () }
    | eof
	{()}
    | _
	{ comment lexbuf }
