##############################
#
# Ryzz PLT Project
#
# Compilation: 
# Option 1: Simply type "make" to compile the Ryzz Language

ryzz: 
	ocamlbuild -pkgs llvm ryzz.native

semantics: 
	ocamlbuild semantic_test.native

parser: 
	ocamlbuild parse_test.native

##############################

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml ast_pp _build/ *.native