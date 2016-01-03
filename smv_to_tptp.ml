open Parsed
open Parser


let rec elem_of_vars vtl =
match vtl with
| [] -> []
| h::tl ->
   match h with
   | Boolean(id) -> id :: elem_of_vars tl
   | Elem(id,_) -> id :: elem_of_vars tl
   | _ -> elem_of_vars tl


let rec from_module_to_var_lists l =
match l with
[] -> []
| h:: tl ->
   match h with
   | Module1(_, varTypeList, _, _)
     -> (elem_of_vars varTypeList)@ from_module_to_var_lists tl
   | Module2(_, _, varTypeList, _)
     -> (elem_of_vars varTypeList)@ from_module_to_var_lists tl

let () =
let chan_in = open_in Sys.argv.(1) in
let lexbuf = Lexing.from_channel chan_in in
while true do
let result = Parser.file Lexer.token lexbuf in
List.iter print_string (from_module_to_var_lists result); 
print_newline();
close_in chan_in
(*print_int result; print_newline(); flush stdout*)
done
