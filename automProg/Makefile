make:
	ocamlc -c syntaxeProg.ml
	menhir --dump -v --infer parserProg.mly
	ocamlc parserProg.mli
	ocamlc -c parserProg.ml
	ocamllex lexerProg.mll
	ocamlc -c lexerProg.ml
	ocamlc -c interpProg.ml
	ocamlc -o mainProg syntaxeProg.cmo lexerProg.cmo parserProg.cmo interpProg.cmo mainProg.ml

clean:
	rm -f *.cmo *.cmi lexerProg.ml parserProg.automaton parserProg.conflicts parserProg.ml parserProg.mli mainProg