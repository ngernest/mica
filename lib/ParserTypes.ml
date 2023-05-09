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

(** ADT representing concrete types *)  
type ty = Int | Char | Bool | Unit | Alpha  
  [@@deriving sexp]

(** Abstract type contained within a module *)  
type abstractType = T | T1 of ty
  [@@deriving sexp]

(** Type representing the arugment to a function *)  
type arg_label = {
  argName : string;
  argType : ty
}
  [@@deriving sexp]

(** Type of an expression *)  
type exprType = Tvar of string | Tarrow1 of arg_label * ty 

(** Record type representing an ML module *)  
type t_module = {
  moduleName : moduleName;
  moduleType : moduleType;
  abstractType : abstractType;
}
[@@deriving sexp]