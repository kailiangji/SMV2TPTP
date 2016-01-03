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

# 50 "parser.ml"
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
  276 (* INIT *);
  277 (* NEXT *);
  278 (* NOT *);
  279 (* OR *);
  280 (* AND *);
  281 (* IMP *);
  282 (* EQUV *);
  283 (* EQ *);
  284 (* COMMA *);
  285 (* COLON *);
  286 (* SEMI *);
  287 (* EQDEF *);
    0 (* EOF *);
  288 (* MODULE *);
  289 (* PROCESS *);
  290 (* BOOLEAN *);
  291 (* VAR *);
  292 (* SPEC *);
  293 (* ASSIGN *);
  294 (* END *);
    0|]

let yytransl_block = [|
  257 (* IDENT *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\005\000\005\000\
\009\000\009\000\009\000\006\000\006\000\006\000\010\000\010\000\
\010\000\010\000\010\000\011\000\011\000\011\000\011\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\007\000\008\000\008\000\
\008\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\002\000\009\000\010\000\000\000\002\000\
\004\000\006\000\008\000\000\000\008\000\008\000\002\000\001\000\
\001\000\003\000\001\000\001\000\001\000\001\000\001\000\004\000\
\004\000\004\000\004\000\004\000\004\000\006\000\006\000\006\000\
\006\000\003\000\003\000\003\000\003\000\003\000\000\000\001\000\
\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\043\000\000\000\000\000\042\000\
\000\000\001\000\004\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\008\000\041\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\009\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\010\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
\006\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\016\000\017\000\000\000\019\000\
\000\000\000\000\000\000\000\000\000\000\011\000\000\000\000\000\
\024\000\025\000\028\000\029\000\026\000\027\000\000\000\000\000\
\000\000\000\000\015\000\021\000\020\000\022\000\023\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\014\000\030\000\032\000\031\000\033\000"

let yydgoto = "\002\000\
\005\000\006\000\007\000\052\000\017\000\031\000\053\000\015\000\
\018\000\089\000\112\000"

let yysindex = "\003\000\
\001\000\000\000\000\000\011\255\000\000\027\000\003\255\000\000\
\247\254\000\000\000\000\011\255\011\255\020\255\042\255\049\255\
\033\255\011\255\011\255\038\255\079\255\094\255\000\000\000\000\
\011\255\011\255\011\255\050\255\076\255\091\255\067\255\070\255\
\092\255\112\255\000\000\011\255\011\255\217\255\094\255\098\255\
\011\255\114\255\128\255\130\255\133\255\134\255\145\255\146\255\
\149\255\148\255\160\255\138\255\111\255\123\255\000\000\162\255\
\144\255\150\255\217\255\217\255\217\255\217\255\217\255\217\255\
\217\255\217\255\017\255\217\255\217\255\217\255\217\255\000\000\
\000\000\161\255\017\255\017\255\127\255\131\255\143\255\147\255\
\159\255\163\255\099\255\107\255\000\000\000\000\017\255\000\000\
\212\255\216\255\040\255\095\255\153\255\000\000\115\255\204\255\
\000\000\000\000\000\000\000\000\000\000\000\000\217\255\217\255\
\217\255\217\255\000\000\000\000\000\000\000\000\000\000\017\255\
\094\255\094\255\173\255\177\255\187\255\191\255\212\255\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\180\000\000\000\
\000\000\000\000\000\000\178\255\155\255\041\255\000\000\000\000\
\000\000\155\255\043\255\000\000\000\000\169\255\000\000\000\000\
\155\255\176\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\168\255\000\000\
\178\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\081\255\089\255\078\255\252\254\044\255\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\255\000\255\000\000\000\000\000\000\000\000\064\255\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\200\000\000\000\252\255\006\000\219\255\237\255\240\255\
\000\000\197\255\000\000"

let yytablesize = 289
let yytable = "\009\000\
\003\000\054\000\024\000\001\000\012\000\036\000\036\000\014\000\
\016\000\033\000\036\000\008\000\036\000\016\000\014\000\095\000\
\096\000\008\000\036\000\036\000\016\000\014\000\034\000\023\000\
\056\000\013\000\010\000\107\000\085\000\086\000\032\000\042\000\
\043\000\036\000\004\000\012\000\014\000\012\000\087\000\077\000\
\078\000\079\000\080\000\081\000\082\000\083\000\084\000\019\000\
\090\000\091\000\092\000\093\000\119\000\037\000\037\000\040\000\
\020\000\039\000\037\000\040\000\037\000\039\000\088\000\069\000\
\070\000\071\000\037\000\037\000\037\000\022\000\088\000\088\000\
\025\000\018\000\018\000\120\000\121\000\021\000\018\000\035\000\
\018\000\037\000\088\000\115\000\116\000\117\000\118\000\035\000\
\035\000\036\000\038\000\038\000\035\000\018\000\035\000\038\000\
\026\000\038\000\034\000\034\000\035\000\018\000\038\000\034\000\
\037\000\034\000\039\000\088\000\103\000\104\000\040\000\027\000\
\028\000\029\000\030\000\035\000\105\000\106\000\038\000\070\000\
\071\000\068\000\069\000\070\000\071\000\041\000\034\000\055\000\
\057\000\068\000\069\000\070\000\071\000\068\000\069\000\070\000\
\071\000\108\000\109\000\110\000\111\000\097\000\058\000\059\000\
\113\000\098\000\060\000\061\000\072\000\068\000\069\000\070\000\
\071\000\068\000\069\000\070\000\071\000\099\000\062\000\063\000\
\073\000\100\000\064\000\065\000\067\000\068\000\069\000\070\000\
\071\000\068\000\069\000\070\000\071\000\101\000\075\000\066\000\
\074\000\102\000\071\000\003\000\076\000\068\000\069\000\070\000\
\071\000\068\000\069\000\070\000\071\000\122\000\094\000\007\000\
\039\000\123\000\039\000\068\000\069\000\070\000\071\000\068\000\
\069\000\070\000\071\000\124\000\012\000\012\000\011\000\125\000\
\000\000\068\000\069\000\070\000\071\000\068\000\069\000\070\000\
\071\000\008\000\044\000\045\000\046\000\047\000\048\000\049\000\
\050\000\051\000\108\000\109\000\110\000\111\000\000\000\000\000\
\000\000\114\000\108\000\109\000\110\000\111\000\068\000\069\000\
\070\000\071\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\004\000"

let yycheck = "\004\000\
\000\000\039\000\019\000\001\000\014\001\010\001\011\001\012\000\
\013\000\026\000\015\001\001\001\017\001\018\000\019\000\075\000\
\076\000\001\001\023\001\024\001\025\000\026\000\027\000\018\000\
\041\000\035\001\000\000\087\000\012\001\013\001\025\000\036\000\
\037\000\038\001\032\001\036\001\041\000\038\001\022\001\059\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\028\001\
\068\000\069\000\070\000\071\000\112\000\010\001\011\001\015\001\
\015\001\015\001\015\001\019\001\017\001\019\001\067\000\024\001\
\025\001\026\001\023\001\024\001\025\001\037\001\075\000\076\000\
\035\001\010\001\011\001\113\000\114\000\029\001\015\001\030\001\
\017\001\038\001\087\000\103\000\104\000\105\000\106\000\010\001\
\011\001\014\001\010\001\011\001\015\001\030\001\017\001\015\001\
\018\001\017\001\010\001\011\001\023\001\038\001\036\001\015\001\
\014\001\017\001\037\001\112\000\010\001\011\001\019\001\033\001\
\034\001\020\001\021\001\038\001\010\001\011\001\038\001\025\001\
\026\001\023\001\024\001\025\001\026\001\014\001\038\001\030\001\
\015\001\023\001\024\001\025\001\026\001\023\001\024\001\025\001\
\026\001\023\001\024\001\025\001\026\001\015\001\015\001\014\001\
\030\001\015\001\014\001\014\001\038\001\023\001\024\001\025\001\
\026\001\023\001\024\001\025\001\026\001\015\001\014\001\014\001\
\038\001\015\001\014\001\016\001\027\001\023\001\024\001\025\001\
\026\001\023\001\024\001\025\001\026\001\015\001\031\001\016\001\
\015\001\015\001\026\001\000\000\031\001\023\001\024\001\025\001\
\026\001\023\001\024\001\025\001\026\001\017\001\030\001\037\001\
\015\001\017\001\019\001\023\001\024\001\025\001\026\001\023\001\
\024\001\025\001\026\001\017\001\036\001\038\001\007\000\017\001\
\255\255\023\001\024\001\025\001\026\001\023\001\024\001\025\001\
\026\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\023\001\024\001\025\001\026\001\255\255\255\255\
\255\255\030\001\023\001\024\001\025\001\026\001\023\001\024\001\
\025\001\026\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\032\001"

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
# 305 "parser.ml"
               : Parsed.file))
; (fun __caml_parser_env ->
    Obj.repr(
# 33 "parser.mly"
     ([])
# 311 "parser.ml"
               : Parsed.file))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'decl) in
    Obj.repr(
# 37 "parser.mly"
       ([_1])
# 318 "parser.ml"
               : 'list_decl))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'decl) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'list_decl) in
    Obj.repr(
# 38 "parser.mly"
                 (_1 :: _2)
# 326 "parser.ml"
               : 'list_decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 7 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 5 : 'varTypeList) in
    let _6 = (Parsing.peek_val __caml_parser_env 3 : 'assignList) in
    let _8 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 43 "parser.mly"
    (Module1(_2,_4,_6,_8))
# 336 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 8 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 6 : 'idents) in
    let _7 = (Parsing.peek_val __caml_parser_env 3 : 'varTypeList) in
    let _9 = (Parsing.peek_val __caml_parser_env 1 : 'assignList) in
    Obj.repr(
# 45 "parser.mly"
 (Module2(_2,_4,_7,_9))
# 346 "parser.ml"
               : 'decl))
; (fun __caml_parser_env ->
    Obj.repr(
# 49 "parser.mly"
                 ([])
# 352 "parser.ml"
               : 'varTypeList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'varType) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'varTypeList) in
    Obj.repr(
# 50 "parser.mly"
                      (_1 :: _2)
# 360 "parser.ml"
               : 'varTypeList))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : 'ident) in
    Obj.repr(
# 54 "parser.mly"
                           (Boolean(_1))
# 367 "parser.ml"
               : 'varType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 2 : 'idents) in
    Obj.repr(
# 55 "parser.mly"
                                  (Elem(_1,_4))
# 375 "parser.ml"
               : 'varType))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 7 : 'ident) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'idents) in
    Obj.repr(
# 56 "parser.mly"
                                             (Process(_1,_4,_6))
# 384 "parser.ml"
               : 'varType))
; (fun __caml_parser_env ->
    Obj.repr(
# 60 "parser.mly"
                     ([])
# 390 "parser.ml"
               : 'assignList))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'assignList) in
    Obj.repr(
# 61 "parser.mly"
                                              (Init(_3,_6)::_8)
# 399 "parser.ml"
               : 'assignList))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 5 : 'ident) in
    let _6 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _8 = (Parsing.peek_val __caml_parser_env 0 : 'assignList) in
    Obj.repr(
# 62 "parser.mly"
                                              (Next(_3,_6)::_8)
# 408 "parser.ml"
               : 'assignList))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 66 "parser.mly"
           (Neg(_2))
# 415 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 67 "parser.mly"
       (True)
# 421 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 68 "parser.mly"
        (False)
# 427 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'infix) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 69 "parser.mly"
                  (Infix(_1,_2,_3))
# 436 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 70 "parser.mly"
        (Elem(_1))
# 443 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
      (Land)
# 449 "parser.ml"
               : 'infix))
; (fun __caml_parser_env ->
    Obj.repr(
# 75 "parser.mly"
     (Lor)
# 455 "parser.ml"
               : 'infix))
; (fun __caml_parser_env ->
    Obj.repr(
# 76 "parser.mly"
      (Limp)
# 461 "parser.ml"
               : 'infix))
; (fun __caml_parser_env ->
    Obj.repr(
# 77 "parser.mly"
       (Leqv)
# 467 "parser.ml"
               : 'infix))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 81 "parser.mly"
                (AX(_3))
# 474 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 82 "parser.mly"
                (EX(_3))
# 481 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 83 "parser.mly"
                (AF(_3))
# 488 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 84 "parser.mly"
                (EF(_3))
# 495 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 85 "parser.mly"
                (AG(_3))
# 502 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 86 "parser.mly"
                (EG(_3))
# 509 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 87 "parser.mly"
                      (AU(_3,_5))
# 517 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 88 "parser.mly"
                      (EU(_3,_5))
# 525 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 89 "parser.mly"
                      (AR(_3,_5))
# 533 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 3 : 'spec) in
    let _5 = (Parsing.peek_val __caml_parser_env 1 : 'spec) in
    Obj.repr(
# 90 "parser.mly"
                      (ER(_3,_5))
# 541 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 91 "parser.mly"
               (Or(_1,_3))
# 549 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 92 "parser.mly"
                (And(_1,_3))
# 557 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 93 "parser.mly"
                (Imp(_1,_3))
# 565 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'spec) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'spec) in
    Obj.repr(
# 94 "parser.mly"
                 (Equv(_1,_3))
# 573 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 95 "parser.mly"
                (Pprop(_1, _3))
# 581 "parser.ml"
               : 'spec))
; (fun __caml_parser_env ->
    Obj.repr(
# 101 "parser.mly"
                  ([])
# 587 "parser.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ident) in
    Obj.repr(
# 102 "parser.mly"
        ([_1])
# 594 "parser.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'ident) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'idents) in
    Obj.repr(
# 103 "parser.mly"
                     (_1::_3)
# 602 "parser.ml"
               : 'idents))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 107 "parser.mly"
      (_1)
# 609 "parser.ml"
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
