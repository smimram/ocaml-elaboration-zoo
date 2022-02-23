all: build

build clean:
	@dune $@

test:
	@dune runtest
