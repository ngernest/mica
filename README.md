# ppx_mica (WIP)

Current progress:
We have two PPX derivers that take an module signature declaration of the form 
(in `bin/main.ml`):
```ocaml
(* bin/main.ml *)
module type SetInterface = sig
  type 'a t 

  val empty : 'a t
  val is_empty : 'a t -> bool
  val mem : 'a -> 'a t -> bool
  ...
end
[@@deriving_inline mica_types, mica] 
(* Auto-generated code is produced here *)
[@@@end]
```
and produces the following type and functor definitions respectively:
```ocaml 
(** Symbolic expressions 
    - Type variables are instantiated with int *)
type expr =
  | Empty
  | Is_empty of expr
  | Mem of int * expr
  ...

(** Types for symbolic expressions *)
type ty = Int | Bool | IntT 

module ExprToImpl(M : SetInterface) = struct ... end 
```

The datatype definitions are produced by the `mica_types` PPX deriver 
which is executed first, and the functor definition is produced by 
the main `mica` deriver which runs afterwards. 

See `lib/lib.ml` for implementation details. 
