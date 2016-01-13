ocamllex lexer.mll
ocamlc -c parsed.ml
ocamlyacc parser.mly
ocamlc -c parser.mli
ocamlc -c lexer.ml
ocamlc -c parser.ml
ocamlc -c smv_to_tptp.ml
ocamlc -o smv_to_tptp lexer.cmo parsed.cmo parser.cmo smv_to_tptp.cmo
