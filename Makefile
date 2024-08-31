.PHONY: default build install uninstall test clean format
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

unit_test: 
	dune runtest test/utils_test	

clean:
	dune clean
	git clean -dfX

format:
	dune build @fmt --auto-promote
