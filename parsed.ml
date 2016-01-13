type var = string
type bvar = string
type name = string

type boolConst = True | False 

type infix =
  | Land | Lor | Limp | Leqv

type lexpr =
  | Neg of lexpr
  | True
  | False
  | Infix of lexpr * infix * lexpr
  | BVar of string

type nlexpr =
  |NBVar of string
  |NBConst of string * string

type var_type =
  |Bvar of var
  |NBvar of var * string list
  |Process of var * name * var list

type assign =
  |BInit of bvar * lexpr 
  |NBInit of var * nlexpr
  |BNext of bvar * lexpr
  |NBNext of var * (nlexpr * nlexpr) list

type spec =
  | Eq of bvar * boolConst
  | AX of spec
  | EX of spec
  | AF of spec
  | EF of spec
  | AG of spec
  | EG of spec
  | AU of spec * spec
  | EU of spec * spec
  | AR of spec * spec
  | ER of spec * spec
  | Or of spec * spec
  | And of spec * spec
  | Imp of spec * spec
  | Equv of spec * spec

type decl =
  | Module1 of name * var_type list * assign list * spec
  | Module2 of name * var list * var_type list * assign list

type file = decl list
