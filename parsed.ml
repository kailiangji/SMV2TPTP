type var = string
type name = string

type var_type = 
  | Boolean of var
  | Elem of var * string list
  | Process of var * name * var list 

type stringValue = string

type infix =
  | Land | Lor | Limp | Leqv

type expr =
  | Neg of expr
  | True
  | False
  | Infix of expr * infix * expr
  | Elem of string

type assign = 
  |Init of var * expr
  |Next of var * expr

type spec =
  | Pprop of string * expr
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
