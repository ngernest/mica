.PHONY: default build install uninstall test clean format coverage
.IGNORE: format

default:
	opam update
	opam install . --deps-only
	dune build

build:
	dune build

install:
	opam update
	opam install --yes . --deps-only

uninstall:
	dune uninstall	

test:
	dune runtest 

clean:
	dune clean
	git clean -dfX

format:
	dune build @fmt --auto-promote

# Make sure `test/ppx_test/errors/dune.inc` is blank 
# before running `make coverage`
coverage:
	make clean
	BISECT_ENABLE=yes dune build
	dune runtest test/ppx_test/passing test/utils_test 
	bisect-ppx-report html
	bisect-ppx-report summary