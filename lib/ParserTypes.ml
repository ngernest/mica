open! Base

(** Name of a module *)
type moduleName = string
  [@@deriving sexp]

(** A module can either be an interface [Intf] or an implementation [Impl]*)  
type moduleType = Intf | Impl
  [@@deriving sexp]

(** Identifiers in OCaml *)  
type ident = string
  [@@deriving sexp]

(** ADT representing concrete types
    [Alpha] represents ['a]
    [AlphaT] represents ['a t]     
    [T] represents [t]
    [Func1] represents functions of arity 1 (arg type, return type)
    [Func2] represents functions of arity 2 (arg1 type, arg2 type, return type)

    TODO: add pairs, lists, options / other parameterized types??

    TODO: add support for types like [int t]
*)  
type ty = Int 
          | Char 
          | Bool 
          | Unit 
          | Alpha 
          | AlphaT
          (* | AlphaT of ty *)
          | T 
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
  | Func1 (a1, b1), Func1 (a2, b2) -> 
    tyEqual a1 a2 && tyEqual b1 b2
  | Func2 (a1, b1, c1), Func2 (a2, b2, c2) -> 
    tyEqual a1 a2 && tyEqual b1 b2 && tyEqual c1 c2
  | _, _ -> false 

(** Converts a [ty] to its string representation 
    Note that [AlphaT] & [T] are converted to "expr" 
    The optional argument [alpha] specifies a concrete base type that should be 
    printed in the PBT code in lieu of ['a\] when testing polymorphic functions. 
    The optional argument [t] specifies a string that should be printed 
    whenever [ty] is equal to [AlphaT] or [T] 
    (eg. the string literal "expr" when generating the [expr] ADT definition). *)
let rec string_of_ty ?(alpha = "\'a") ?(t = "expr") (ty : ty) : string = 
  match ty with 
  | Int -> "int"
  | Char -> "char"
  | Bool -> "bool"
  | Unit -> "unit"
  | Alpha -> alpha
  | AlphaT | T -> t
  | Func1 (arg, ret) -> 
    String.concat ~sep:" -> " (List.map ~f: string_of_ty [arg; ret])
  | Func2 (arg1, arg2, ret) -> 
    String.concat ~sep:" -> " (List.map ~f: string_of_ty [arg1; arg2; ret])

(** Abstract type contained within a module *)  
type abstractType = T0 | T1 of ty
  [@@deriving sexp]

(** Type representing a value declaration *)   
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

(** TODO: handle parens in arrow types *)
(** TODO: figure out how to ignore comments?*)
