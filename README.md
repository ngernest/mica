# ppx_mica (WIP)

Current progress:
We have a PPX deriver that takes an module signature declaration of the form (see `main.ml`):
```ocaml
(* bin/main.ml *)
module type SetInterface = sig
  type 'a t 

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : 'a -> 'a t -> bool
  ...
end
[@@deriving mica] 
```
and produces the following algebraic data type definitions:
```ocaml 
(** Symbolic expressions *)
type expr =
  | Empty
  | Is_empty of expr
  | Mem of int * expr
  ...

(** Types for symbolic expressions *)
type ty = Int | Bool | IntT  
```
(you can see this new definition through VS Code's "Generate mli file" functionality).
Note that the ['a] type parameter has been instantiated with [int]. 

See `lib/lib.ml` for the details of how this is implemented.
