# ppx_accessors

(Code adapted from the [ppxlib Github repo](https://github.com/ocaml-ppx/ppxlib/tree/main/examples/simple-extension-rewriter))

This folder contains an example of a very simple ppx deriver that will generate
accessors for record fields from the record type definition.
**Note**: still need to get the executable to work in Utop  (see [Running in Utop](#running-in-utop) section below)

E.g. the following:

```ocaml
type t =
  { a : string
  ; b : int
  }
  [@@deriving accessors]
```

will generate the following:

```ocaml
let a x = x.a
let b x = x.b
```

It can also be used in `.mli` files to generate the corresponding signatures:

```ocaml
val a : t -> string
val b : t -> int
```

To use the deriver, run `dune build`, and then run the following in the terminal:
```shell
$ dune exec -- ./example/main.exe

a x = hello
b x = 5
```

## Running in Utop (Work in progress)
To test in a OCaml REPL, be sure to use `utop-full` instead of `utop` 
(only the former works with OCaml's compiler libraries).

After starting up `utop-full`, load in the dependencies in the following order:
```ocaml
#require "ppxlib";;
#require "ppxlib.metaquot";;
#require "ppx_deriving";;

#use "example/main.ml";;
```
See https://discuss.ocaml.org/t/adding-attributes-using-ppxlib-metaquot/9142/8 for more

**TODO**: figure out why this doesn't work in Utop 
