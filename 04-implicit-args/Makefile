all: test

test:
	@dune exec ./eval.exe example.txt

explain:
	# menhir --explain parser.mly && less parser.conflicts && rm parser.conflicts parser.ml parser.mli
	ocamlc -c common.ml extlib.ml preterm.ml term.ml && menhir --infer --explain parser.mly && rm *.cmi *.cmo parser.ml parser.mli && less parser.conflicts && rm parser.conflicts
