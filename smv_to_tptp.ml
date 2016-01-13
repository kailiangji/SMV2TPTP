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
       ("b(" ^ bvar ^ ")" ) :: list_of_str_of_vars tl md_lst
     else
       ("b(" ^ var_md ^ "_" ^ bvar ^")" ) :: list_of_str_of_vars ~var_md:(var_md) tl md_lst
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


let state_var_list md md_lst=
  match md with
  | Module1(nam, vars, _,_) -> list_of_str_of_vars vars md_lst
  | _ -> raise Not_Main_Module


let () =
let chan_in = open_in Sys.argv.(1) in
let lexbuf = Lexing.from_channel chan_in in
let result = Parser.file Lexer.token lexbuf in
List.iter (fun x -> print_string (x^"\n")) (state_var_list (List.hd result) (List.tl result)); 
print_newline();
close_in chan_in
(*print_int result; print_newline(); flush stdout*)

