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

(** Abstract type contained within a module *)  
type abstractType = T | T1 
  [@@deriving sexp]


(** Record type representing an ML module *)  
type t_module = {
  moduleName : moduleName;
  moduleType : moduleType;
  abstractType : abstractType;
}
[@@deriving sexp]