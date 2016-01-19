ocamllex lexer.mll
ocamlc -c  -annot parsed.ml
ocamlyacc parser.mly
ocamlc -c  -annot parser.mli
ocamlc -c  -annot lexer.ml
ocamlc -c  -annot parser.ml
ocamlc -c -annot smv_to_tptp.ml
ocamlc -o smv_to_tptp lexer.cmo parsed.cmo parser.cmo smv_to_tptp.cmo
