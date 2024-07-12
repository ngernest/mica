open Ppx_mica__Type_deriver
open Astlib.Pprintast
open Boilerplate
open Alcotest

let loc = Location.none

(******************************************************************************)
(* Tests for [gen_atom] *)
let gen_atom_int () =
  let expected = "quickcheck_generator_int" in
  let actual = gen_atom ~loc [%type: int] |> string_of_expression in
  mk_test string "int" expected actual

let gen_atom_char () =
  let expected = "quickcheck_generator_char" in
  let actual = gen_atom ~loc [%type: char] |> string_of_expression in
  mk_test string "char" expected actual

let gen_atom_string () =
  let expected = "quickcheck_generator_string" in
  let actual = gen_atom ~loc [%type: string] |> string_of_expression in
  mk_test string "string" expected actual

let gen_atom_int_list () =
  let expected =
    [%expr quickcheck_generator_list quickcheck_generator_int]
    |> string_of_expression in
  let actual = gen_atom ~loc [%type: int list] |> string_of_expression in
  mk_test string "int list" expected actual

let gen_atom_char_option () =
  let expected =
    [%expr quickcheck_generator_option quickcheck_generator_char]
    |> string_of_expression in
  let actual = gen_atom ~loc [%type: char option] |> string_of_expression in
  mk_test string "char option" expected actual

let gen_atom_tuple2 () =
  let expected =
    [%expr tuple2 quickcheck_generator_int quickcheck_generator_char]
    |> string_of_expression in
  let actual = gen_atom ~loc [%type: int * char] |> string_of_expression in
  mk_test string "int * char" expected actual

let gen_atom_tuple3 () =
  let expected =
    [%expr
      tuple3 quickcheck_generator_int quickcheck_generator_char
        quickcheck_generator_string]
    |> string_of_expression in
  let actual =
    gen_atom ~loc [%type: int * char * string] |> string_of_expression in
  mk_test string "int * char" expected actual

let gen_atom_option_list_pair () =
  let expected =
    [%expr
      tuple2
        (quickcheck_generator_list quickcheck_generator_int)
        (quickcheck_generator_option quickcheck_generator_char)]
    |> string_of_expression in
  let actual =
    gen_atom ~loc [%type: int list * char option] |> string_of_expression in
  mk_test string "(int list) * (char option)" expected actual

let gen_atom_nested_list () =
  let expected =
    [%expr
      quickcheck_generator_list
        (quickcheck_generator_list quickcheck_generator_int)]
    |> string_of_expression in
  let actual = gen_atom ~loc [%type: int list list] |> string_of_expression in
  mk_test string "(int list) list" expected actual

let gen_atom_expr_recursive_call () =
  let expected =
    [%expr with_size ~size:(k / 2) (gen_expr T)] |> string_of_expression in
  let actual = gen_atom ~loc [%type: expr] |> string_of_expression in
  mk_test string "expr" expected actual
