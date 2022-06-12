make:
	ocamlc -c syntaxe.ml
	menhir --dump -v --infer parser.mly
	ocamlc parser.mli
	ocamlc -c parser.ml
	ocamllex lexer.mll
	ocamlc -c lexer.ml
	ocamlc -c interp.ml
	ocamlc -o main syntaxe.cmo lexer.cmo parser.cmo interp.cmo main.ml

clean:
	rm -f *.cmo *.cmi lexer.ml parser.automaton parser.conflicts parser.ml parser.mli main