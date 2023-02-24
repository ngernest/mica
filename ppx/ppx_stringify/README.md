# ppx_stringify

(Code adapted from the OCaml Explore "Meta-programming with PPX" [tutorial](https://ocaml-explore.netlify.app/workflows/meta-programming-with-ppx/#writing-a-ppx-deriver) -- this README 
is also adapted from the tutorial)

## Motivation
Consider the problem of generating string producing functions for OCaml types.      
For example:
```ocaml
# string_of_int
- : int -> string = <fun>
# let int_list_stringify lst = "[" ^ List.fold_left (fun acc s -> acc ^ (string_of_int s) ^ ";") "" lst ^ "]"
val int_list_stringify : int list -> string = <fun>
# int_list_stringify [1;2;3;4]
- : string = "[1;2;3;4;]"
```
Manually writing `*_list_stringfy` functions is a laborious process. Ideally,
a PPX deriver would be able to automatically generate these functions for us!


## Description of folder
This folder contains a PPX deriver that derives "stringify" functions straight 
from type definitions.For succinctness, we will focus on making functions for simple 
types like `int` and `t list`.

To use the deriver, run `dune build`, and then run the following in the terminal:
```shell
$ dune exec -- ./example/main.exe
[1;2;3;]
[true;false;true;]
[[1;2;3;];[4;5;6;];]
```
