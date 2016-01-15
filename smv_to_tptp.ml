open Parsed
open Parser

let str_of_md2_name f var_lst =
  match var_lst with
  | [] -> f^"()"
  | [h] -> f^"("^h^")"
  | h::tl -> f^"("^
     (List.fold_left (fun a b -> a^","^b) h tl)
     ^")"

exception Cannot_Find_Module of string

let rec find_mdl m_nam m_vars md_lst=
  match md_lst with
  | [] -> raise (Cannot_Find_Module (str_of_md2_name m_nam m_vars))
  | (Module2(nam, vars, var_dec_ls, _)) as h::tl ->
     if nam = m_nam then h
     else find_mdl m_nam m_vars tl
  | _ ::tl -> find_mdl m_nam m_vars tl
       

exception Not_Main_Module
exception Main_Module
 
let rec lst_str_of_vars ?var_md:(var_md ="") vars md_lst =
  match vars with
  | [] -> []
  | (Boolean(bvar)) :: tl ->
     if var_md = "" then
        bvar :: lst_str_of_vars tl md_lst
     else
       (var_md ^ "_" ^ bvar ) :: lst_str_of_vars ~var_md:(var_md) tl md_lst
  | (Elem(elm, _ )) :: tl -> 
     if var_md = ""then
       elm :: lst_str_of_vars tl md_lst
     else     
       (var_md^"_"^elm) :: lst_str_of_vars ~var_md:(var_md) tl md_lst
  | (Proc(name, st, st_lst)) :: tl -> 
     match find_mdl st st_lst md_lst with
     | Module2(_,_,var_decl_lst, _) -> 
	(lst_str_of_vars ~var_md:(name) var_decl_lst md_lst) @ 
	  lst_str_of_vars tl md_lst
     | _ -> raise Main_Module

let rec lst_str_of_vars' ?var_md:(var_md ="") vars md_lst =
  match vars with
  | [] -> []
  | (Boolean(bvar)) as h :: tl ->
     if var_md = "" then
       h :: lst_str_of_vars' tl md_lst
     else
      Boolean(var_md ^ "_" ^ bvar) :: lst_str_of_vars' ~var_md:(var_md) tl md_lst
  | (Elem(elm, value_lst )) as h :: tl -> 
     if var_md = ""then
       h :: lst_str_of_vars' tl md_lst
     else     
      Elem(var_md^"_"^elm, value_lst) :: lst_str_of_vars' ~var_md:(var_md) tl md_lst
  | (Proc(name, st, st_lst)) :: tl -> 
     match find_mdl st st_lst md_lst with
     | Module2(_,_,var_decl_lst, _) -> 
	(lst_str_of_vars' ~var_md:(name) var_decl_lst md_lst) @ 
	  lst_str_of_vars' tl md_lst
     | _ -> raise Main_Module

let rec print_vars_with_types vars =
  match vars with
  | [] -> ()
  | h1::((h2::tl') as tl) ->( match h1 with
    | Boolean(v) -> print_string ("Boolean(" ^ v ^ "), ");
      print_vars_with_types tl
    | Elem(elm,_) -> print_string ("Elem("^elm^"), ");
      print_vars_with_types tl
    | _ -> print_vars_with_types tl
  )
  | [h] -> ( match h with
    | Boolean(v) -> print_string ("Boolean(" ^ v ^ ")");
    | Elem(elm,_) -> print_string ("Elem(" ^ elm ^ ")");
    | _ -> ()
  )

let rec print_vars_without_types vars =
  match vars with
  | [] -> ()
  | h1::((h2::tl') as tl) ->( match h1 with
    | Boolean(v) -> print_string (v ^ ", ");
      print_vars_without_types tl
    | Elem(elm,_) -> print_string (elm^", ");
      print_vars_without_types tl
    | _ -> print_vars_without_types tl
  )
  | [h] -> ( match h with
    | Boolean(v) -> print_string ( v );
    | Elem(elm,_) -> print_string ( elm );
    | _ -> ()
  )

let state_var_list md md_lst=
  match md with
  | Module1(nam, vars, _,_) -> lst_str_of_vars vars md_lst
  | _ -> raise Not_Main_Module

let state_var_list' md md_lst=
  match md with
  | Module1(nam, vars, _,_) -> lst_str_of_vars' vars md_lst
  | _ -> raise Not_Main_Module

exception Not_Initialized
exception Not_Bool_Exp

type exp_val =
  | Bool of bool
  | Const of string

let neg_value exp =
  match exp with
  | Bool(true) -> Bool(false)
  | Bool(false) -> Bool(true)
  | _ -> raise Not_Bool_Exp

let or_value e1 e2 =
  match e1 with
  | Bool(b1) -> (match e2 with
    | Bool(b2) -> let b = (b1 || b2) in Bool(b)
    | _ -> raise Not_Bool_Exp)
  | _ -> raise Not_Bool_Exp


let and_value e1 e2 =
  match e1 with
  | Bool(b1) -> (match e2 with
    | Bool(b2) -> let b = (b1 && b2) in Bool(b)
    | _ -> raise Not_Bool_Exp)
  | _ -> raise Not_Bool_Exp

let rec value_of_exp exp =
  match exp with
  | True -> Bool(true)
  | False -> Bool(false)
  | Neg(exp') -> neg_value (value_of_exp exp')
  | Or(exp1,exp2) -> or_value (value_of_exp exp1) (value_of_exp exp2)
  | And(exp1,exp2) -> and_value (value_of_exp exp1) (value_of_exp exp2)
  | Eq(exp1,exp2) -> 
     if (value_of_exp exp1) = (value_of_exp exp2) then Bool(true)
     else Bool(false)
  | Var(str) -> Const(str)

let str_of_exp exp =
  let value = value_of_exp exp in
  match value with
  | Bool(true) -> "b(tt,ff)"
  | Bool(false) -> "b(ff,tt)"
  | Const(st) -> st

let rec list_of_inits_of_vars vars assigns md_lst =
  match vars with
  | [] -> []
  | h :: tl -> (find_value h assigns md_lst) @ 
     list_of_inits_of_vars tl assigns md_lst
and
find_value var assigns md_lst =
  match var with
  | Boolean(bvar) ->(
     match assigns with
     | [] -> raise Not_Initialized
     | Init(bvar', exp) :: tl -> 
	if bvar=bvar' then [Boolean(str_of_exp exp)]
	else find_value var tl md_lst
     | _ :: tl -> find_value var tl md_lst )
  | Elem(evar, val_lst) ->(
     match assigns with
     | [] -> raise Not_Initialized
     | Init(evar', exp) :: tl -> 
	if evar=evar' then [Elem(str_of_exp exp, val_lst)]
	else find_value var tl md_lst
     | _ :: tl -> find_value var tl md_lst )
  | Proc(name, st, st_lst) ->
     match find_mdl st st_lst md_lst with
     | Module2(_,_,vars', assigns') -> 
	list_of_inits_of_vars vars' assigns' md_lst
     | _ -> raise Main_Module


let init_state md md_lst =
  match md with
  | Module1(name, vars, assigns, spec) ->
     list_of_inits_of_vars vars assigns md_lst
  | _ -> raise Not_Main_Module

let put_ahead_elem_vars var_lst =
  let rec put_ahead_elem_vars' acc1 acc2 var_lst =
    match var_lst with
    | [] -> acc1 @ acc2
    | h::tl -> match h with
      | Boolean(_) -> put_ahead_elem_vars' acc1 (acc2@[h]) tl
      | _ -> put_ahead_elem_vars' (acc1@[h]) acc2 tl
  in put_ahead_elem_vars' [] [] var_lst
  

let rec false_var_state v var_l =
  match var_l with
  | [] -> ""
  | h1::((h2::tl') as tl) -> 
     if v=h1 then 
       "b(ff,tt), "^ false_var_state v tl
     else
       (String.uppercase h1)^", "^false_var_state v tl
  | [h] -> if v=h then "b(ff,tt) " else String.uppercase h

let rec true_var_state v var_l =
  match var_l with
  | [] -> ""
  | h1::((h2::tl') as tl) -> 
     if v=h1 then 
       "b(tt,ff), "^ true_var_state v tl
     else
       (String.uppercase h1)^", "^true_var_state v tl
  | [h] -> if v=h then "b(tt,ff) " else String.uppercase h

let rec atomic_prop_in_state' var_l1 var_l2 =
  match var_l1 with
  | [] -> print_string "\n"
  | h::tl -> match h with
    | Boolean(v) -> 
       print_string 
	 ("cnf(" ^ v ^ "f, axiom, ~pi0("^ v ^", s("
       ^ (false_var_state v var_l2) ^ "))).\n" 
       ^ "cnf(" ^ v ^ "t, axiom,  pi0("^ v ^", s("
       ^ (true_var_state v var_l2) ^ "))).\n");
      atomic_prop_in_state' tl var_l2
    |_ -> ()
 

let atomic_prop_in_state md var_lst =
  match md with
  | Module1(_, vars, _, _) ->
     atomic_prop_in_state' vars var_lst
  | _ -> raise Not_Main_Module

exception Not_Consider_Yet

let rec str_of_spec spec =
  match spec with
    | AU(spec1, spec2) -> "au(" ^ (str_of_spec spec1) ^ "," 
     ^ (str_of_spec spec2) ^ ")"
  | EU(spec1, spec2) -> "eu(" ^ (str_of_spec spec1) ^ "," 
     ^ (str_of_spec spec2) ^ ")"
  | AR(spec1, spec2) -> "ar(" ^ (str_of_spec spec1) ^ "," 
     ^ (str_of_spec spec2) ^ ")"
  | ER(spec1, spec2) -> "er(" ^ (str_of_spec spec1) ^ "," 
     ^ (str_of_spec spec2) ^ ")"
  | And(spec1, spec2) -> "and(" ^ (str_of_spec spec1) ^ "," 
     ^ (str_of_spec spec2) ^ ")"
  | Or(spec1, spec2) -> "or(" ^ (str_of_spec spec1) ^ "," 
     ^ (str_of_spec spec2) ^ ")"
  | Neg(spec1) -> "not("^(str_of_spec spec1) ^ ")"
  | Eq(var, exp) -> (
     match value_of_exp exp with
     | Bool(true) -> var
     | Bool(false) -> "not(" ^ var ^ ")"
     | _ -> raise Not_Consider_Yet )
  | AX(spec1) -> "ax(" ^ (str_of_spec spec1) ^ ")"
  | EX(spec1) -> "ex(" ^ (str_of_spec spec1) ^ ")"
  | AF(spec1) -> "af(" ^ (str_of_spec spec1) ^ ")"
  | EF(spec1) -> "ef(" ^ (str_of_spec spec1) ^ ")"
  | AG(spec1) -> "ag(" ^ (str_of_spec spec1) ^ ")"
  | EG(spec1) -> "eg(" ^ (str_of_spec spec1) ^ ")"

let spec_in_main md =
  match md with
  | Module1(_, _, _, spec) ->
     str_of_spec spec
  | _ -> raise Not_Main_Module


let state_shape var_lst =
  print_string "s(";
  print_vars_without_types var_lst;
  print_string ")"

let goal spec s =
print_string "cnf(check, negated_conjecture, pi0(";
print_string (spec^", ");
state_shape s;
print_string ")."

(*let succ_of_a_state s md_lst =*)

let () =
let chan_in = open_in Sys.argv.(1) in
let lexbuf = Lexing.from_channel chan_in in
let result = Parser.file Lexer.token lexbuf in
let inits = init_state (List.hd result) (List.tl result) in
let spec = spec_in_main (List.hd result) in
let inits' = put_ahead_elem_vars inits in
let var_lst = state_var_list (List.hd result) (List.tl result) in
let var_lst' = state_var_list' (List.hd result) (List.tl result) in
let var_lst'' = put_ahead_elem_vars var_lst' in
List.iter (fun x -> print_string (x^"\n")) var_lst;
print_vars_with_types var_lst';
print_newline();
print_vars_with_types var_lst'';
print_newline();
state_shape var_lst'';
print_newline();
atomic_prop_in_state (List.hd result) var_lst;
print_newline();
goal spec inits';
print_newline();
close_in chan_in
(*print_int result; print_newline(); flush stdout*)

(*cnf(p1f, axiom, ~pi0(p1, s(C1, C2, b(ff,tt), B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12))).*)
