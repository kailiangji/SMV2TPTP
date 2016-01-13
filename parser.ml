type token =
  | IDENT of (string)
  | AX
  | EX
  | AG
  | EG
  | AF
  | EF
  | A
  | E
  | U
  | R
  | TRUE
  | FALSE
  | LP
  | RP
  | LB
  | RB
  | LCB
  | RCB
  | CASE
  | ESAC
  | INIT
  | NEXT
  | NOT
  | OR
  | AND
  | IMP
  | EQUV
  | EQ
  | COMMA
  | COLON
  | SEMI
  | EQDEF
  | EOF
  | MODULE
  | PROCESS
  | BOOLEAN
  | VAR
  | SPEC
  | ASSIGN
  | END

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Lexing
open Parsing
open Parsed

# 52 "parser.ml"
let yytransl_const = [|
  258 (* AX *);
  259 (* EX *);
  260 (* AG *);
  261 (* EG *);
  262 (* AF *);
  263 (* EF *);
  264 (* A *);
  265 (* E *);
  266 (* U *);
  267 (* R *);
  268 (* TRUE *);
  269 (* FALSE *);
  270 (* LP *);
  271 (* RP *);
  272 (* LB *);
  273 (* RB *);
  274 (* LCB *);
  275 (* RCB *);
  276 (* CASE *);
  277 (* ESAC *);
  278 (* INIT *);
  279 (* NEXT *);
  280 (* NOT *);
  281 (* OR *);
  282 (* AND *);
  283 (* IMP *);
  284 (* EQUV *);
  285 (* EQ *);
  286 (* COMMA *);
  287 (* COLON *);
  288 (* SEMI *);
  289 (* EQDEF *);
    0 (* EOF *);
  290 (* MODULE *);
  291 (* PROCESS *);
  292 (* BOOLEAN *);
  293 (* VAR *);
  294 (* SPEC *);
  295 (* ASSIGN *);
  296 (* END *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\005\000\005\000\
\009\000\009\000\009\000\006\000\006\000\010\000\010\000\010\000\
\012\000\012\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\008\000\008\000\008\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\002\000\008\000\009\000\000\000\003\000\
\003\000\005\000\007\000\000\000\003\000\006\000\006\000\008\000\
\000\000\005\000\003\000\002\000\003\000\003\000\003\000\004\000\
\004\000\004\000\004\000\004\000\004\000\006\000\006\000\006\000\
\006\000\001\000\001\000\002\000\003\000\003\000\003\000\001\000\
\000\000\001\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\045\000\000\000\000\000\044\000\
\000\000\001\000\004\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\043\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\008\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\010\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\013\000\006\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\
\000\000\000\000\000\000\011\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\021\000\034\000\
\035\000\000\000\040\000\000\000\000\000\000\000\000\000\000\000\
\000\000\024\000\025\000\028\000\029\000\026\000\027\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\016\000\030\000\032\000\031\000\033\000\000\000\000\000\
\018\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\056\000\017\000\031\000\057\000\015\000\
\018\000\032\000\112\000\113\000"

let yysindex = "\021\000\
\007\000\000\000\000\000\007\255\000\000\047\000\033\255\000\000\
\245\254\000\000\000\000\007\255\007\255\039\255\074\255\064\255\
\058\255\059\255\007\255\084\255\045\255\055\255\007\255\000\000\
\007\255\007\255\007\255\000\000\147\255\150\255\088\255\096\255\
\000\000\108\255\146\255\152\255\007\255\007\255\052\255\055\255\
\055\255\000\000\007\255\153\255\154\255\156\255\157\255\158\255\
\159\255\160\255\161\255\151\255\162\255\052\255\052\255\148\255\
\098\255\000\000\000\000\164\255\143\255\149\255\052\255\052\255\
\052\255\052\255\052\255\052\255\052\255\052\255\049\255\000\000\
\038\255\052\255\052\255\000\000\038\255\022\255\067\255\075\255\
\079\255\105\255\107\255\110\255\060\255\062\255\000\000\000\000\
\000\000\038\255\000\000\133\255\098\255\155\255\133\255\038\255\
\133\255\000\000\000\000\000\000\000\000\000\000\000\000\052\255\
\052\255\052\255\052\255\163\255\038\255\038\255\038\255\114\255\
\165\255\251\254\112\255\127\255\129\255\133\255\077\255\133\255\
\038\255\000\000\000\000\000\000\000\000\000\000\131\255\038\255\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\180\000\000\000\
\000\000\000\000\000\000\168\255\145\255\009\255\000\000\000\000\
\000\000\000\000\026\255\000\000\000\000\166\255\145\255\000\000\
\145\255\169\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\009\000\000\000\168\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\011\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\065\000\068\000\052\000\167\255\170\255\
\171\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\027\000\015\000\040\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\170\255\
\000\000"

let yygindex = "\000\000\
\000\000\178\000\000\000\006\000\247\255\101\000\044\000\243\255\
\000\000\000\000\183\255\059\000"

let yytablesize = 358
let yytable = "\092\000\
\012\000\036\000\012\000\095\000\097\000\024\000\003\000\008\000\
\012\000\009\000\005\000\123\000\035\000\033\000\037\000\034\000\
\108\000\014\000\016\000\074\000\075\000\001\000\008\000\042\000\
\014\000\013\000\038\000\042\000\016\000\060\000\016\000\014\000\
\036\000\088\000\089\000\118\000\119\000\120\000\008\000\039\000\
\041\000\096\000\044\000\045\000\041\000\090\000\010\000\127\000\
\014\000\088\000\089\000\022\000\008\000\046\000\047\000\048\000\
\049\000\050\000\051\000\052\000\053\000\090\000\026\000\087\000\
\019\000\054\000\004\000\023\000\019\000\104\000\105\000\106\000\
\107\000\074\000\075\000\055\000\029\000\030\000\091\000\027\000\
\028\000\098\000\091\000\091\000\074\000\075\000\074\000\075\000\
\020\000\099\000\023\000\074\000\075\000\100\000\021\000\091\000\
\022\000\071\000\072\000\074\000\075\000\091\000\110\000\074\000\
\075\000\111\000\079\000\080\000\081\000\082\000\083\000\084\000\
\085\000\086\000\091\000\091\000\091\000\093\000\094\000\101\000\
\025\000\102\000\074\000\075\000\103\000\039\000\091\000\040\000\
\124\000\074\000\075\000\074\000\075\000\091\000\074\000\075\000\
\074\000\075\000\109\000\110\000\058\000\059\000\111\000\125\000\
\121\000\126\000\041\000\114\000\115\000\116\000\117\000\074\000\
\075\000\074\000\075\000\109\000\110\000\109\000\110\000\111\000\
\037\000\111\000\128\000\038\000\042\000\043\000\069\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\077\000\
\073\000\070\000\076\000\003\000\075\000\078\000\041\000\007\000\
\011\000\122\000\129\000\041\000\000\000\000\000\017\000\111\000\
\000\000\000\000\000\000\000\000\000\000\000\000\014\000\000\000\
\000\000\000\000\015\000\012\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\036\000\000\000\000\000\000\000\
\036\000\000\000\036\000\000\000\000\000\000\000\000\000\000\000\
\037\000\037\000\036\000\036\000\000\000\037\000\000\000\037\000\
\036\000\036\000\012\000\036\000\038\000\038\000\012\000\037\000\
\004\000\038\000\012\000\038\000\005\000\037\000\037\000\000\000\
\037\000\039\000\039\000\000\000\000\000\000\000\039\000\000\000\
\039\000\038\000\038\000\000\000\038\000\022\000\022\000\000\000\
\000\000\000\000\022\000\000\000\022\000\000\000\039\000\039\000\
\000\000\039\000\019\000\019\000\022\000\023\000\023\000\019\000\
\000\000\019\000\023\000\000\000\023\000\022\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\000\000\023\000"

let yycheck = "\073\000\
\000\000\000\000\014\001\077\000\078\000\019\000\000\000\001\001\
\000\000\004\000\000\000\017\001\026\000\023\000\000\000\025\000\
\090\000\012\000\013\000\025\001\026\001\001\000\001\001\015\001\
\019\000\037\001\000\000\019\001\023\000\043\000\025\000\026\000\
\027\000\012\001\013\001\109\000\110\000\111\000\001\001\000\000\
\015\001\020\001\037\000\038\000\019\001\024\001\000\000\121\000\
\043\000\012\001\013\001\000\000\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\024\001\018\001\015\001\
\000\000\014\001\034\001\000\000\030\001\010\001\011\001\010\001\
\011\001\025\001\026\001\024\001\022\001\023\001\073\000\035\001\
\036\001\015\001\077\000\078\000\025\001\026\001\025\001\026\001\
\015\001\015\001\032\001\025\001\026\001\015\001\031\001\090\000\
\039\001\054\000\055\000\025\001\026\001\096\000\026\001\025\001\
\026\001\029\001\063\000\064\000\065\000\066\000\067\000\068\000\
\069\000\070\000\109\000\110\000\111\000\074\000\075\000\015\001\
\037\001\015\001\025\001\026\001\015\001\038\001\121\000\032\001\
\017\001\025\001\026\001\025\001\026\001\128\000\025\001\026\001\
\025\001\026\001\025\001\026\001\040\000\041\000\029\001\017\001\
\031\001\017\001\039\001\104\000\105\000\106\000\107\000\025\001\
\026\001\025\001\026\001\025\001\026\001\025\001\026\001\029\001\
\014\001\029\001\032\001\014\001\019\001\014\001\016\001\015\001\
\015\001\014\001\014\001\014\001\014\001\014\001\014\001\033\001\
\029\001\016\001\015\001\000\000\026\001\033\001\015\001\039\001\
\007\000\021\001\128\000\019\001\255\255\255\255\021\001\029\001\
\255\255\255\255\255\255\255\255\255\255\255\255\032\001\255\255\
\255\255\255\255\032\001\038\001\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\010\001\011\001\255\255\255\255\255\255\
\015\001\255\255\017\001\255\255\255\255\255\255\255\255\255\255\
\010\001\011\001\025\001\026\001\255\255\015\001\255\255\017\001\
\031\001\032\001\034\001\034\001\010\001\011\001\038\001\025\001\
\034\001\015\001\034\001\017\001\034\001\031\001\032\001\255\255\
\034\001\010\001\011\001\255\255\255\255\255\255\015\001\255\255\
\017\001\031\001\032\001\255\255\034\001\010\001\011\001\255\255\
\255\255\255\255\015\001\255\255\017\001\255\255\031\001\032\001\
\255\255\034\001\010\001\011\001\025\001\010\001\011\001\015\001\
\255\255\017\001\015\001\255\255\017\001\034\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\034\001\255\255\255\255\034\001"

let yynames_const = "\
  AX\000\
  EX\000\
  AG\000\
  EG\000\
  AF\000\
  EF\000\
  A\000\
  E\000\
  U\000\
  R\000\
  TRUE\000\
  FALSE\000\
  LP\000\
  RP\000\
  LB\000\
  RB\000\
  LCB\000\
  RCB\000\
  CASE\000\
  ESAC\000\
  INIT\000\
  NEXT\000\
  NOT\000\
  OR\000\
  AND\000\
  IMP\000\
  EQUV\000\
  EQ\000\
  COMMA\000\
  COLON\000\
  SEMI\000\
  EQDEF\000\
  EOF\000\
  MODULE\000\
  PROCESS\000\
  BOOLEAN\000\
  VAR\000\
  SPEC\000\
  ASSIGN\000\
  END\000\
  "

let yynames_block = "\
  IDENT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'list_decl) in
    Obj.repr(
# 32 "parser.mly"
               (_1)
# 330 "parser.ml"
               : Parsed.file))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "parser.mly"
     ([])
# 336 "parser.ml"
               : Parsed.file))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 37 "parser.mly"
       ([_1])
# 343 "parser.ml"
               : 'list_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_decl) in
    Obj.repr(
# 38 "parser.mly"
                 (_1 :: _2)
# 351 "parser.ml"
               : 'list_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'var_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'assign_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 43 "parser.mly"
    (Module1(_2,_4, _6, _8))
# 361 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'idents) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'var_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'assign_list) in
    Obj.repr(
# 45 "parser.mly"
 (Module2(_2,_4,_7, _9))
# 371 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                 ([])
# 377 "parser.ml"
               : 'var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'var_list) in
    Obj.repr(
# 50 "parser.mly"
                    (_1::_3)
# 385 "parser.ml"
               : 'var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    Obj.repr(
# 54 "parser.mly"
                      (Boolean(_1))
# 392 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'idents) in
    Obj.repr(
# 55 "parser.mly"
                             (Elem(_1,_4))
# 400 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'idents) in
    Obj.repr(
# 56 "parser.mly"
                                         (Proc(_1,_4,_6))
# 409 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
                 ([])
# 415 "parser.ml"
               : 'assign_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assign) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assign_list) in
    Obj.repr(
# 61 "parser.mly"
                          (_1::_3)
# 423 "parser.ml"
               : 'assign_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 65 "parser.mly"
                             (Init(_3, _6))
# 431 "parser.ml"
               : 'assign))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 66 "parser.mly"
                             (Next(_3,_6))
# 439 "parser.ml"
               : 'assign))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'case_assigns) in
    Obj.repr(
# 67 "parser.mly"
                                                (CNext(_3, _7))
# 447 "parser.ml"
               : 'assign))
; (fun __caml_parser_env ->
    Obj.repr(
# 71 "parser.mly"
                  ([])
# 453 "parser.ml"
               : 'case_assigns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'case_assigns) in
    Obj.repr(
# 72 "parser.mly"
                                  ((_1, _3)::_5)
# 462 "parser.ml"
               : 'case_assigns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 76 "parser.mly"
               (Eq(_1, _3))
# 470 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 77 "parser.mly"
           (Neg(_2))
# 477 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 78 "parser.mly"
             (_2)
# 484 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 79 "parser.mly"
                (And(_1, _3))
# 492 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 80 "parser.mly"
               (Or(_1, _3))
# 500 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 81 "parser.mly"
                (AX(_3))
# 507 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 82 "parser.mly"
                (EX(_3))
# 514 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 83 "parser.mly"
                (AF(_3))
# 521 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 84 "parser.mly"
                (EF(_3))
# 528 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 85 "parser.mly"
                (AG(_3))
# 535 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 86 "parser.mly"
                (EG(_3))
# 542 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 87 "parser.mly"
                      (AU(_3,_5))
# 550 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 88 "parser.mly"
                      (EU(_3,_5))
# 558 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 89 "parser.mly"
                      (AR(_3,_5))
# 566 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 90 "parser.mly"
                      (ER(_3,_5))
# 574 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    Obj.repr(
# 94 "parser.mly"
       (True)
# 580 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
        (False)
# 586 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 96 "parser.mly"
          ( Neg(_2))
# 593 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 97 "parser.mly"
              (And(_1, _3))
# 601 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 98 "parser.mly"
             (Or(_1, _3))
# 609 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 99 "parser.mly"
             (Eq(_1, _3))
# 617 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 100 "parser.mly"
        (Var(_1))
# 624 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 105 "parser.mly"
                  ([])
# 630 "parser.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 106 "parser.mly"
        ([_1])
# 637 "parser.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'idents) in
    Obj.repr(
# 107 "parser.mly"
                     (_1::_3)
# 645 "parser.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 111 "parser.mly"
      (_1)
# 652 "parser.ml"
               : 'ident))
(* Entry file *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let file (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Parsed.file)
