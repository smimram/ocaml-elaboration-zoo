all: test

test:
	@dune exec ./eval.exe example.txt

explain:
	menhir --explain parser.mly && less parser.conflicts && rm parser.conflicts parser.ml parser.mli
