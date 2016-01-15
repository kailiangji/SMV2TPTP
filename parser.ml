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
\000\000\005\000\003\000\002\000\003\000\003\000\003\000\002\000\
\002\000\002\000\002\000\002\000\002\000\006\000\006\000\006\000\
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
\000\000\013\000\006\000\000\000\000\000\000\000\024\000\025\000\
\028\000\029\000\026\000\027\000\000\000\000\000\000\000\020\000\
\000\000\000\000\000\000\011\000\000\000\000\000\000\000\000\000\
\021\000\034\000\035\000\000\000\040\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\016\000\030\000\032\000\
\031\000\033\000\000\000\000\000\018\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\056\000\017\000\031\000\057\000\015\000\
\018\000\032\000\100\000\101\000"

let yysindex = "\005\000\
\009\000\000\000\000\000\012\255\000\000\020\000\250\254\000\000\
\245\254\000\000\000\000\012\255\012\255\017\255\027\255\049\255\
\050\255\086\255\012\255\057\255\242\254\041\255\012\255\000\000\
\012\255\012\255\012\255\000\000\129\255\130\255\092\255\113\255\
\000\000\107\255\128\255\134\255\012\255\012\255\105\255\041\255\
\041\255\000\000\012\255\135\255\136\255\105\255\105\255\105\255\
\105\255\105\255\105\255\133\255\137\255\105\255\105\255\123\255\
\044\255\000\000\000\000\139\255\122\255\124\255\000\000\000\000\
\000\000\000\000\000\000\000\000\105\255\105\255\020\255\000\000\
\115\255\105\255\105\255\000\000\115\255\073\255\040\255\062\255\
\000\000\000\000\000\000\115\255\000\000\127\255\044\255\132\255\
\095\255\115\255\095\255\105\255\105\255\105\255\105\255\127\255\
\115\255\115\255\115\255\109\255\138\255\042\255\100\255\106\255\
\116\255\095\255\235\254\127\255\115\255\000\000\000\000\000\000\
\000\000\000\000\066\255\115\255\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\160\000\000\000\
\000\000\000\000\000\000\146\255\125\255\015\255\000\000\000\000\
\000\000\000\000\056\255\000\000\000\000\131\255\125\255\000\000\
\125\255\143\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\
\011\000\000\000\146\255\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\014\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\040\000\078\000\060\000\
\140\255\142\255\141\255\000\000\000\000\000\000\000\000\002\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\052\000\027\000\015\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\142\255\000\000"

let yygindex = "\000\000\
\000\000\158\000\000\000\006\000\016\000\096\000\007\000\237\255\
\000\000\000\000\195\255\050\000"

let yytablesize = 368
let yytable = "\024\000\
\012\000\036\000\012\000\026\000\098\000\001\000\035\000\099\000\
\003\000\009\000\012\000\086\000\008\000\005\000\039\000\089\000\
\091\000\014\000\016\000\010\000\027\000\028\000\096\000\060\000\
\014\000\013\000\037\000\004\000\016\000\042\000\016\000\014\000\
\036\000\042\000\081\000\106\000\107\000\108\000\033\000\019\000\
\034\000\020\000\044\000\045\000\074\000\075\000\019\000\115\000\
\014\000\092\000\093\000\038\000\063\000\064\000\065\000\066\000\
\067\000\068\000\111\000\022\000\071\000\072\000\029\000\030\000\
\074\000\075\000\074\000\075\000\074\000\075\000\041\000\094\000\
\095\000\008\000\041\000\079\000\080\000\023\000\085\000\021\000\
\087\000\088\000\085\000\085\000\082\000\083\000\074\000\075\000\
\022\000\085\000\097\000\098\000\090\000\025\000\099\000\085\000\
\084\000\116\000\102\000\103\000\104\000\105\000\085\000\085\000\
\085\000\008\000\046\000\047\000\048\000\049\000\050\000\051\000\
\052\000\053\000\085\000\008\000\112\000\023\000\054\000\097\000\
\098\000\085\000\113\000\099\000\074\000\075\000\082\000\083\000\
\055\000\039\000\074\000\075\000\114\000\097\000\098\000\058\000\
\059\000\099\000\084\000\109\000\074\000\075\000\037\000\038\000\
\040\000\041\000\042\000\043\000\069\000\061\000\062\000\073\000\
\070\000\076\000\077\000\099\000\078\000\075\000\110\000\003\000\
\041\000\041\000\017\000\007\000\011\000\117\000\000\000\000\000\
\012\000\000\000\000\000\014\000\015\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\036\000\036\000\000\000\000\000\000\000\
\036\000\000\000\036\000\000\000\000\000\000\000\000\000\000\000\
\039\000\039\000\036\000\036\000\000\000\039\000\000\000\039\000\
\036\000\036\000\012\000\036\000\037\000\037\000\012\000\039\000\
\039\000\037\000\004\000\037\000\012\000\039\000\039\000\005\000\
\039\000\019\000\019\000\037\000\000\000\000\000\019\000\000\000\
\019\000\037\000\037\000\000\000\037\000\038\000\038\000\000\000\
\019\000\019\000\038\000\000\000\038\000\022\000\022\000\000\000\
\000\000\019\000\022\000\000\000\022\000\000\000\000\000\000\000\
\000\000\000\000\038\000\038\000\022\000\038\000\000\000\023\000\
\023\000\000\000\000\000\000\000\023\000\022\000\023\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000"

let yycheck = "\019\000\
\000\000\000\000\014\001\018\001\026\001\001\000\026\000\029\001\
\000\000\004\000\000\000\073\000\001\001\000\000\000\000\077\000\
\078\000\012\000\013\000\000\000\035\001\036\001\084\000\043\000\
\019\000\037\001\000\000\034\001\023\000\015\001\025\000\026\000\
\027\000\019\001\015\001\097\000\098\000\099\000\023\000\000\000\
\025\000\015\001\037\000\038\000\025\001\026\001\030\001\109\000\
\043\000\010\001\011\001\000\000\046\000\047\000\048\000\049\000\
\050\000\051\000\017\001\000\000\054\000\055\000\022\001\023\001\
\025\001\026\001\025\001\026\001\025\001\026\001\015\001\010\001\
\011\001\001\001\019\001\069\000\070\000\000\000\073\000\031\001\
\074\000\075\000\077\000\078\000\012\001\013\001\025\001\026\001\
\039\001\084\000\025\001\026\001\020\001\037\001\029\001\090\000\
\024\001\032\001\092\000\093\000\094\000\095\000\097\000\098\000\
\099\000\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\109\000\001\001\017\001\032\001\014\001\025\001\
\026\001\116\000\017\001\029\001\025\001\026\001\012\001\013\001\
\024\001\038\001\025\001\026\001\017\001\025\001\026\001\040\000\
\041\000\029\001\024\001\031\001\025\001\026\001\014\001\014\001\
\032\001\039\001\019\001\014\001\016\001\015\001\015\001\029\001\
\016\001\015\001\033\001\029\001\033\001\026\001\021\001\000\000\
\015\001\019\001\021\001\039\001\007\000\116\000\255\255\255\255\
\038\001\255\255\255\255\032\001\032\001\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
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
\026\001\015\001\034\001\017\001\034\001\031\001\032\001\034\001\
\034\001\010\001\011\001\025\001\255\255\255\255\015\001\255\255\
\017\001\031\001\032\001\255\255\034\001\010\001\011\001\255\255\
\025\001\026\001\015\001\255\255\017\001\010\001\011\001\255\255\
\255\255\034\001\015\001\255\255\017\001\255\255\255\255\255\255\
\255\255\255\255\031\001\032\001\025\001\034\001\255\255\010\001\
\011\001\255\255\255\255\255\255\015\001\034\001\017\001\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\034\001"

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
# 33 "parser.mly"
               (_1)
# 326 "parser.ml"
               : Parsed.file))
; (fun __caml_parser_env ->
    Obj.repr(
# 34 "parser.mly"
     ([])
# 332 "parser.ml"
               : Parsed.file))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 38 "parser.mly"
       ([_1])
# 339 "parser.ml"
               : 'list_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_decl) in
    Obj.repr(
# 39 "parser.mly"
                 (_1 :: _2)
# 347 "parser.ml"
               : 'list_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'var_list) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'assign_list) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 44 "parser.mly"
    (Module1(_2,_4, _6, _8))
# 357 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'idents) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'var_list) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : 'assign_list) in
    Obj.repr(
# 46 "parser.mly"
 (Module2(_2,_4,_7, _9))
# 367 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 50 "parser.mly"
                 ([])
# 373 "parser.ml"
               : 'var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'var) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'var_list) in
    Obj.repr(
# 51 "parser.mly"
                    (_1::_3)
# 381 "parser.ml"
               : 'var_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    Obj.repr(
# 55 "parser.mly"
                      (Boolean(_1))
# 388 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 1 : 'idents) in
    Obj.repr(
# 56 "parser.mly"
                             (Elem(_1,_4))
# 396 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 6 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : 'idents) in
    Obj.repr(
# 57 "parser.mly"
                                         (Proc(_1,_4,_6))
# 405 "parser.ml"
               : 'var))
; (fun __caml_parser_env ->
    Obj.repr(
# 61 "parser.mly"
                 ([])
# 411 "parser.ml"
               : 'assign_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'assign) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'assign_list) in
    Obj.repr(
# 62 "parser.mly"
                          (_1::_3)
# 419 "parser.ml"
               : 'assign_list))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 66 "parser.mly"
                             (Init(_3, _6))
# 427 "parser.ml"
               : 'assign))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 67 "parser.mly"
                             (Next(_3,_6))
# 435 "parser.ml"
               : 'assign))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : 'case_assigns) in
    Obj.repr(
# 68 "parser.mly"
                                                (CNext(_3, _7))
# 443 "parser.ml"
               : 'assign))
; (fun __caml_parser_env ->
    Obj.repr(
# 72 "parser.mly"
                  ([])
# 449 "parser.ml"
               : 'case_assigns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : 'case_assigns) in
    Obj.repr(
# 73 "parser.mly"
                                  ((_1, _3)::_5)
# 458 "parser.ml"
               : 'case_assigns))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 77 "parser.mly"
               (Eq(_1, _3))
# 466 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 78 "parser.mly"
           (Neg(_2))
# 473 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 79 "parser.mly"
             (_2)
# 480 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 80 "parser.mly"
                (And(_1, _3))
# 488 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 81 "parser.mly"
               (Or(_1, _3))
# 496 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 82 "parser.mly"
          (AX(_2))
# 503 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 83 "parser.mly"
          (EX(_2))
# 510 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 84 "parser.mly"
          (AF(_2))
# 517 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 85 "parser.mly"
          (EF(_2))
# 524 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 86 "parser.mly"
          (AG(_2))
# 531 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 87 "parser.mly"
          (EG(_2))
# 538 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 88 "parser.mly"
                      (AU(_3,_5))
# 546 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 89 "parser.mly"
                      (EU(_3,_5))
# 554 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 90 "parser.mly"
                      (AR(_3,_5))
# 562 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 91 "parser.mly"
                      (ER(_3,_5))
# 570 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    Obj.repr(
# 95 "parser.mly"
       (True)
# 576 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 96 "parser.mly"
        (False)
# 582 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 97 "parser.mly"
          ( Neg(_2))
# 589 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 98 "parser.mly"
              (And(_1, _3))
# 597 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 99 "parser.mly"
             (Or(_1, _3))
# 605 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 100 "parser.mly"
             (Eq(_1, _3))
# 613 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 101 "parser.mly"
        (Var(_1))
# 620 "parser.ml"
               : 'exp))
; (fun __caml_parser_env ->
    Obj.repr(
# 106 "parser.mly"
                  ([])
# 626 "parser.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 107 "parser.mly"
        ([_1])
# 633 "parser.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'idents) in
    Obj.repr(
# 108 "parser.mly"
                     (_1::_3)
# 641 "parser.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 112 "parser.mly"
      (_1)
# 648 "parser.ml"
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
