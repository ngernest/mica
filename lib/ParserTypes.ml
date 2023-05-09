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

    TODO: add pairs & lists / parameterized types??
*)  
type ty = Int 
          | Char 
          | Bool 
          | Unit 
          | Alpha 
          | AlphaT 
          | T 
          | Func1 of ty * ty 
          | Func2 of ty * ty * ty
          [@@deriving sexp]

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

(** TODO: figure out how to pretty-print s-expresisons*)