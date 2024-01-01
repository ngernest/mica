# ppx_mica (WIP)

Current progress:
We have a PPX deriver that takes an module signature declaration of the form (see `main.ml`):
```ocaml
(* bin/main.ml *)
module type S = sig 
  type 'a t 
end
[@@deriving mica]
```
and produces a new type definition of the form:
```ocaml 
type expr = T of int 
```
(you can see this new definition through VS Code's "Generate mli file" functionality).
Note that the ['a] type parameter has been instantiated with [int]. 

See `lib/lib.ml` for the details of how this is implemented.
