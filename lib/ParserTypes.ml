open Base
open Utils

(** Datatype definitions for OCaml module signatures. 
    These types are used by the module {!Lib.ModuleParser} to parse 
    OCaml module signatures. This file also calls some helper 
    functions defined in the module {!Lib.Utils}. *)

(** Name of a module *)
type moduleName = string
  [@@deriving sexp]

(** A module can either be an interface [Intf] or an implementation [Impl]*)  
type moduleType = Intf | Impl
  [@@deriving sexp]

(** Identifiers in OCaml *)  
type ident = string
  [@@deriving sexp]

(** [ty] is an ADT representing concrete types, where: 
    - [Alpha] represents ['a]
    - [AlphaT] represents ['a t]     
    - [T] represents [t]
    - [Option] represents option types 
    - [Pair] represents a {i pair}, i.e. a product type with two arguments
    - [List] represents lists
    - [Func1] represents functions of arity 1 (arg type, return type)
    - [Func2] represents functions of arity 2 (arg1 type, arg2 type, return type) 
    - All other constructors correspond to base OCaml types *)  
type ty = Int 
          | Char 
          | Bool 
          | Unit 
          | Alpha 
          | AlphaT
          | T 
          | Option of ty
          | Pair of ty * ty
          | List of ty
          | Func1 of ty * ty 
          | Func2 of ty * ty * ty
  [@@deriving sexp, compare]

(** Structural equality function for the [ty] datatype *)  
let rec tyEqual (ty1 : ty) (ty2 : ty) : bool = 
  match ty1, ty2 with 
  | Int, Int
  | Char, Char
  | Bool, Bool
  | Unit, Unit
  | Alpha, Alpha
  | AlphaT, AlphaT
  | T, T -> true
  | Option t1, Option t2 -> tyEqual t1 t2 
  | Func1 (a1, b1), Func1 (a2, b2) -> 
    tyEqual a1 a2 && tyEqual b1 b2
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
    eg. ["int option"]. 
*)
let rec string_of_ty ?(alpha = "\'a") ?(t = "expr") ?(camelCase = false) (ty : ty) : string = 
  match ty with 
  | Int -> "int"
  | Char -> "char"
  | Bool -> "bool"
  | Unit -> "unit"
  | Alpha -> alpha
  | AlphaT | T -> t
  | Option ty -> 
    let t = string_of_ty ~alpha ~t ty in 
    if camelCase then t ^ "Option"
    else t ^ " option"
  | Pair (t1, t2) -> 
    let (t1', t2') = map2 ~f:(string_of_ty ~alpha ~t ~camelCase) (t1, t2) in 
    t1' ^ " * " ^ t2'
  | List eltTy -> string_of_ty ~alpha ~t ~camelCase eltTy ^ " list"
  | Func1 (arg, ret) -> 
    String.concat ~sep:" -> " (List.map ~f: string_of_ty [arg; ret])
  | Func2 (arg1, arg2, ret) -> 
    String.concat ~sep:" -> " (List.map ~f: string_of_ty [arg1; arg2; ret])

(** Abstract type contained within a module 
    - The [T0] constructor means the abstract type is {i not} parameterized by any type variables, i.e. the abstract type is just [t] 
    - The [T1] constructor takes a type [ty] as its argument, representing an abstract type containing a type variable, 
    eg. ['a t]  *)  
type abstractType = T0 | T1 of ty
  [@@deriving sexp]

(** Type representing a value declaration:
    - [valName] is the name of the variable/function being declared
    - [valType] is the type of the declaration
    - E.g. for [val empty : 'a t], [valName = "empty"] and [valType = AlphaT] *)   
type valDecl = {
  valName : string;
  valType : ty
}
  [@@deriving sexp, fields]

(** Type representing the argument to a function *)   
type argLabel = valDecl
  [@@deriving sexp]

(** Record type representing an ML module *)  
type moduleSig = {
  moduleName : moduleName;
  moduleType : moduleType;
  abstractType : abstractType;
  valDecls : valDecl list [@sexp.list]
}
[@@deriving sexp, fields]

(* TODO: handle parens in arrow types *)
(* TODO: figure out how to ignore comments in parser *)
