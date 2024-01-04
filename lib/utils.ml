open Ppxlib 
open Ast_helper
open Ast_builder.Default

open StdLabels

(******************************************************************************)
(** {1 Utility functions} *)

(** Retrieves all elements of a list except the last one *)  
let rec remove_last (lst : 'a list) : 'a list = 
  match lst with 
  | [] | [_] -> [] 
  | x :: xs -> x :: remove_last xs
  
(** List of OCaml base types 
    - The named argument [loc] is necessary in order for 
    the [Ppxlib.Metaquot] quotations to expand to the appropriate 
    AST fragments representing the base types. *)  
let base_types ~(loc : location) : core_type list = [
  [%type: int];
  [%type: int32];
  [%type: int64];
  [%type: nativeint];
  [%type: char]; 
  [%type: bool];
  [%type: unit];
  [%type: float];
  [%type: string] 
]  

(** Instantiates all type variables ['a] inside a type expression with [int] 
    by recursing over the structure of the type expression. 
    Base types are left unchanged. 
    Note: this function only recurses over type expressions when 
    they consist of:
    - Type constructor applications ([Ptyp_constr])
    - Tuples ([Ptyp_tuple])
    - Arrow/function types ([Ptyp_arrow]). *)  
let rec monomorphize (ty : core_type) : core_type = 
  let loc = ty.ptyp_loc in  
  match ty.ptyp_desc with 
  | ty_desc when List.mem ty ~set:(base_types ~loc) -> ty
  | Ptyp_var _ -> [%type: int] 
  | Ptyp_arrow (arg_lbl, t1, t2) -> 
    { ty with ptyp_desc = 
      Ptyp_arrow (arg_lbl, monomorphize t1, monomorphize t2) }
  | Ptyp_tuple tys -> 
    { ty with ptyp_desc = 
      Ptyp_tuple (List.map ~f:monomorphize tys) }
  | Ptyp_constr (ident, ty_params) -> 
    { ty with ptyp_desc = 
      Ptyp_constr (ident, List.map ~f:monomorphize ty_params) } 
  | _ -> ty

(** [get_type_varams td] extracts the type parameters 
    from the type declaration [td]
    - Type variables (e.g. ['a]) are instantiated with [int] *)  
let get_type_params (td : type_declaration) : core_type list = 
  List.map td.ptype_params ~f:(fun (core_ty, _) -> monomorphize core_ty)

(** [mkError ~local ~global msg] creates an error extension node, 
    associated with an element in the AST at the location [local],
    and reports the error message [msg] at the location [global] *)      
let mkError ~(local : location) ~(global : location) msg : structure_item = 
  let ext = Location.error_extensionf ~loc:local msg in 
  pstr_extension ~loc:global ext []      

(** Name of the abstract type in the module signature, 
    by default ["t"] *)  
let abstract_ty_name : string ref = ref "t"  

(* Note: this currently doesn't work *)
let attrs : attributes ref = ref []

