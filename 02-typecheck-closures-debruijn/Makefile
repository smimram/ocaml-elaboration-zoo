all: test

test:
	dune exec ./eval.exe ex0.txt
	dune exec ./eval.exe ex1.txt
	dune exec ./eval.exe ex2.txt
	dune exec ./eval.exe ex3.txt

explain:
	menhir --explain parser.mly && less parser.conflicts && rm parser.conflicts parser.ml parser.mli
