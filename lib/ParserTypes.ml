open Base
open Utils

(* Disable "unused-values" compiler warnings *)
[@@@ocaml.warning "-32-34-27"]

(** Datatype definitions for OCaml module signatures. 
    These types are used by the module [Lib.ModuleParser] to parse 
    OCaml module signatures. This file also calls some helper 
    functions defined in the module [Lib.Utils]. *)

type moduleName = string [@@deriving sexp]
(** Name of a module *)

(** A module can either be an interface [Intf] or an implementation [Impl] *)
type moduleType = Intf | Impl [@@deriving sexp]

type ident = string [@@deriving sexp]
(** Identifiers in OCaml *)

(** [ty] is an ADT representing concrete types, where: 
    - [Alpha] represents ['a]
    - [AlphaT] represents ['a t]     
    - [T] represents [t]
    - [Option] represents option types 
    - [Pair] represents a {i pair}, i.e. a product type with two arguments
    - [List] represents lists
    - [Func1] represents functions of arity 1 (arg type, return type)
    - [Func2] represents functions of arity 2 (arg1 type, arg2 type, return type) 
    - [NamedAbstract] represents a {i named} monomorphic abstract type, i.e. an abstact type
      that is not [t], e.g. the type [private_key] in the Diffie-Hellman example. 
      Note: polymorphic named abstract types are currently not supported. 
    - [Opaque] represents an opaque type defined in an external module. 
    An example of an opaque type is [AssocList.t], where [AssocList] is an externally 
    defined module (defined in a file [AssocList.ml] in the [lib] directory). 
    We require that the type in this external module be called [t], and that the 
    QuickCheck generator should be called [quickcheck_generator], 
    following the naming conventions of 
    Jane Street's {{: https://github.com/janestreet/base_quickcheck/tree/master/ppx_quickcheck} ppx_quickcheck}
    library. 
    - See [lib/maps/AssocList.ml] for an example of opaque types in use. 
    - All other constructors correspond to base OCaml types *)
type ty =
  | Int
  | Char
  | Bool
  | Unit
  | String
  | Alpha
  | AlphaT
  | T
  | NamedAbstract of string
  | Opaque of string
  | Option of ty
  | Pair of ty * ty
  | List of ty
  | Func1 of ty * ty
  | Func2 of ty * ty * ty
[@@deriving sexp, compare]

(** Abstract type contained within a module 
  - The [T0] constructor means the abstract type is not parameterized by any type variables, i.e. the abstract type is just [t] 
  - The [string] argument for the [T0] constructor is the name of the abstract type 
    (eg. ["t"] or ["private_key"] for the Diffie-Hellman example)
  - The [T1] constructor takes a type [ty] as its argument, representing an abstract type containing a type variable, eg. ['a t]  
  - [T1] also takes in a [string] argument, which is the name of the abstract type (eg. ["t"])*)
type abstractType = T0 of string | T1 of ty * string [@@deriving sexp]

(** Structural equality function for the [ty] datatype *)
let rec tyEqual (ty1 : ty) (ty2 : ty) : bool =
  match (ty1, ty2) with
  | Int, Int
  | Char, Char
  | Bool, Bool
  | Unit, Unit
  | Alpha, Alpha
  | AlphaT, AlphaT
  | T, T ->
      true
  | Opaque s1, Opaque s2 -> String.equal s1 s2
  | Option t1, Option t2 -> tyEqual t1 t2
  | Func1 (a1, b1), Func1 (a2, b2) -> tyEqual a1 a2 && tyEqual b1 b2
  | Func2 (a1, b1, c1), Func2 (a2, b2, c2) ->
      tyEqual a1 a2 && tyEqual b1 b2 && tyEqual c1 c2
  | _, _ -> false

(** Converts a [ty] to its string representation, taking in optional arguments
    specifying various parameters of the string represntation. 
    - Note that [AlphaT] & [T] are converted to "expr" 
    - The argument [alpha] specifies a concrete base type that should be 
    printed in the PBT code in lieu of [ \'a ] when testing polymorphic functions. 
    - The argument [t] specifies a string that should be printed 
    whenever [ty] is equal to [AlphaT] or [T] 
    (eg. the string literal ["expr"] when generating the [expr] ADT definition)
    - The argument [camelCase] specifies whether the 
    string representation of [ty] should be in camel-case or not. This is relevant
    for parameterized types whose string representation may contain a space, 
    eg. ["int option"]. *)
let rec string_of_ty ?(alpha = "\'a") ?(t = "expr") ?(camelCase = false)
    (ty : ty) : string =
  let open String in
  let sep = if camelCase then "" else " " in
  match ty with
  | Int -> "int"
  | Char -> "char"
  | Bool -> "bool"
  | Unit -> "unit"
  | String -> "string"
  | Alpha -> alpha
  | AlphaT | T -> t
  | NamedAbstract s -> s
  | Opaque s ->
      if camelCase then split ~on:'.' s |> List.map ~f:capitalize |> concat
      else s
  (* TODO: figure out if we need to parenthesize parameterized types?
     (and if so, how to avoid cases where we have parens appearing in
      the derived constructor names
      -- maybe only parenthesize if [camelCase] is false) *)
  | Option ty ->
      let tyStr = string_of_ty ~alpha ~t ty in
      if camelCase then concat ~sep @@ (tyStr :: [ "Option" ])
      else parensStr @@ concat ~sep @@ (tyStr :: [ "option" ])
  | Pair (t1, t2) ->
      let s1, s2 = map2 ~f:(string_of_ty ~alpha ~t ~camelCase) (t1, t2) in
      if camelCase then concat ~sep @@ List.map ~f:capitalize [ s1; s2; "Pair" ]
      else parensStr (s1 ^ " * " ^ s2)
  | List eltTy ->
      let tyStr = string_of_ty ~alpha ~t ~camelCase eltTy in
      if camelCase then concat ~sep @@ (tyStr :: [ "List" ])
      else parensStr @@ concat ~sep @@ (tyStr :: [ "list" ])
  | Func1 (arg, ret) ->
      concat ~sep:" -> "
        (List.map ~f:(string_of_ty ~alpha ~t ~camelCase) [ arg; ret ])
  | Func2 (arg1, arg2, ret) ->
      concat ~sep:" -> "
        (List.map ~f:(string_of_ty ~alpha ~t ~camelCase) [ arg1; arg2; ret ])

type valDecl = { valName : string; valType : ty } [@@deriving sexp, fields]
(** Type representing a value declaration:
    - [valName] is the name of the variable/function being declared
    - [valType] is the type of the declaration
    - E.g. for [val empty : 'a t], [valName = "empty"] and [valType = AlphaT] *)

(** Auxiliary type which specifies whether integer QuickCheck generators
    should generate:
    - [AllInts] (i.e. all positive/negative ints) 
    - [NonNegativeOnly] (only generate non-negative ints) 
    - [PositiveOnly] (only generate strictly positive ints) 
    - Note: when a module signature is parsed in the [ModuleParser] module, 
      by default, [intFlag] is set to [AllInts] *)
type intFlag = AllInts | NonNegativeOnly | PositiveOnly [@@deriving sexp]

type moduleSig = {
  moduleName : moduleName;
  moduleType : moduleType;
  abstractTypes : abstractType list;
  valDecls : valDecl list; [@sexp.list]
  intFlag : intFlag;
}
[@@deriving sexp, fields]
(** Record type representing an ML module *)

(* TODO: handle parens in arrow types *)
(* TODO: figure out how to ignore comments in parser *)
