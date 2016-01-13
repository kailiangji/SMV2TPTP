type var = string

type exp =
  | True
  | False
  | Neg of exp
  | And of exp * exp
  | Or of exp * exp
  | Eq of exp * exp
  | Var of string

type condition =
  | Otherwise
  | Eq of string * string

type assign =
  | Init of var * exp
  | Next of var * exp
  | CNext of var * (exp * exp) list

type var_decl =
  | Boolean of string
  | Elem of string * string list
  | Proc of string * string * string list

type spec =
  | Eq of string * exp
  | Neg of spec
  | And of spec * spec
  | Or of spec * spec
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

type decl =
  | Module1 of string * var_decl list * assign list * spec
  | Module2 of string * string list * var_decl list * assign list


type file = decl list
