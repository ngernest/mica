# Frontend to dune
# Adapted from https://github.com/mjambon/dune-starter/tree/master

.PHONY: default build install uninstall clean

default: build

build:
	dune build

install:
	opam install ./ --deps-only

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