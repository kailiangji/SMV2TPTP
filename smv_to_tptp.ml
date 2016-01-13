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
 
let rec list_of_str_of_vars ?var_md:(var_md ="") vars md_lst =
  match vars with
  | [] -> []
  | (Boolean(bvar)) :: tl ->
     if var_md = "" then
        bvar :: list_of_str_of_vars tl md_lst
     else
       (var_md ^ "_" ^ bvar ) :: list_of_str_of_vars ~var_md:(var_md) tl md_lst
  | (Elem(elm, _ )) :: tl -> 
     if var_md = ""then
       elm :: list_of_str_of_vars tl md_lst
     else     
       (var_md^"_"^elm) :: list_of_str_of_vars ~var_md:(var_md) tl md_lst
  | (Proc(name, st, st_lst)) :: tl -> 
     match find_mdl st st_lst md_lst with
     | Module2(_,_,var_decl_lst, _) -> 
	(list_of_str_of_vars ~var_md:(name) var_decl_lst md_lst) @ 
	  list_of_str_of_vars tl md_lst
     | _ -> raise Main_Module

let rec list_of_str_of_vars' ?var_md:(var_md ="") vars md_lst =
  match vars with
  | [] -> []
  | (Boolean(bvar)) :: tl ->
     if var_md = "" then
       ("b(" ^ bvar ^ ")" ) :: list_of_str_of_vars' tl md_lst
     else
       ("b(" ^ var_md ^ "_" ^ bvar ^")" ) :: list_of_str_of_vars' ~var_md:(var_md) tl md_lst
  | (Elem(elm, _ )) :: tl -> 
     if var_md = ""then
       elm :: list_of_str_of_vars' tl md_lst
     else     
       (var_md^"_"^elm) :: list_of_str_of_vars' ~var_md:(var_md) tl md_lst
  | (Proc(name, st, st_lst)) :: tl -> 
     match find_mdl st st_lst md_lst with
     | Module2(_,_,var_decl_lst, _) -> 
	(list_of_str_of_vars' ~var_md:(name) var_decl_lst md_lst) @ 
	  list_of_str_of_vars' tl md_lst
     | _ -> raise Main_Module


let state_var_list md md_lst=
  match md with
  | Module1(nam, vars, _,_) -> list_of_str_of_vars vars md_lst
  | _ -> raise Not_Main_Module

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


let () =
let chan_in = open_in Sys.argv.(1) in
let lexbuf = Lexing.from_channel chan_in in
let result = Parser.file Lexer.token lexbuf in
let var_lst = state_var_list (List.hd result) (List.tl result) in
List.iter (fun x -> print_string (x^"\n")) var_lst;
atomic_prop_in_state (List.hd result) var_lst;
print_newline();
close_in chan_in
(*print_int result; print_newline(); flush stdout*)

(*cnf(p1f, axiom, ~pi0(p1, s(C1, C2, b(ff,tt), B2, B3, B4, B5, B6, B7, B8, B9, B10, B11, B12))).*)
