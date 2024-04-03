##############################
#
# Ryzz PLT Project
#
# Compilation: 
# Option 1: Simply type "make" to compile the Ryzz Language


%.cmo : %.ml
	ocamlc -w A -c $<

%.cmi : %.mli
	ocamlc -w A -c $<

ast_pp : ast_pp.ml ast.cmo
	ocamlc -w A -o ast_pp $^

scanner.ml : scanner.mll
	ocamllex $^

parser.ml parser.mli : parser.mly
	ocamlyacc $^

# Depedencies from ocamldep
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : parser.cmi
scanner.cmx : parser.cmx

##############################

.PHONY : clean
clean :
	rm -rf *.cmi *.cmo parser.ml parser.mli scanner.ml ast_pp