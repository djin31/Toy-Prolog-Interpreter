all: interpreter.cmo parser.cmo lexer.cmo  prolog.cmo
	ocamlfind ocamlc -linkpkg -package unix -o run interpreter.cmo parser.cmo lexer.cmo prolog.cmo

test: interpreter.cmo tester.cmo
	ocamlfind ocamlc -linkpkg -package unix -o tester interpreter.cmo tester.cmo

interpreter.cmo:
	ocamlc -c interpreter.ml

tester.cmo:
	ocamlc -c tester.ml

parser.cmo:
	ocamlyacc parser.mly
	ocamlc -c parser.mli
	ocamlc -c parser.ml

prolog.cmo:
	ocamlc -c prolog.ml

lexer.cmo:
	ocamllex lexer.mll
	ocamlc -c lexer.ml

clean:
	rm -f lexer.ml parser.ml 
	rm -f *.cmi
	rm -f *.cmo
	rm -f *.mli
	rm -f tester
	rm -f run
	