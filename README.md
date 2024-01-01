# ppx_mica (WIP)

Current progress:
We have a PPX deriver that takes an abstract type definition of the form (see `main.ml`):
```ocaml
(* bin/main.ml *)
type t [@@deriving expr]
```
and produces a new type definition of the form:
```ocaml 
type expr = T
```
(you can see this new definition through VS Code's "Generate mli file" functionality).

See `lib/lib.ml` for the details of how this is implemented.
