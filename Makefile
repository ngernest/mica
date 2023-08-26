# Frontend to dune
# Taken from https://github.com/mjambon/dune-starter/tree/master

.PHONY: default build install uninstall test clean fmt
.IGNORE: fmt

default: build

build:
	dune build

install:
	dune install

uninstall:
	dune uninstall

# Remove any files that were automatically produced by Mica on a previous run
clean-mica:
	rm -f lib/generated.ml
	rm -f bin/compare_impls.ml
	touch bin/compare_impls.ml

clean:
	rm -f lib/generated.ml
	rm -f bin/compare_impls.ml
	touch bin/compare_impls.ml
	dune clean
	

fmt:
	dune build @fmt
	dune promote