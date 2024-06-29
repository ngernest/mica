open Ppxlib
open Ast_helper
open Ast_builder.Default
open StdLabels
open Equality
open Lident
open Miscellany
open Printers
open Getters
open Builders

(******************************************************************************)
(** {1 Working with [module_expr]s} *)

(******************************************************************************)
(** {1 Utility functions for working with Ppxlib} *)

(** Returns true the abstract type declaration in a [signature] 
    is parameterized (e.g. ['a t]), else returns [false] *)
let rec is_abs_ty_parameterized (sig_items : signature) : bool =
  List.fold_left
    ~f:(fun acc { psig_desc; _ } ->
      match psig_desc with
      | Psig_type (_rec_flag, ty_decls) -> (
        match ty_decls with
        | [] -> acc
        | _ ->
          list_or
          @@ List.map
               ~f:(fun { ptype_name; ptype_params; _ } ->
                 String.equal ptype_name.txt "t"
                 && not (list_is_empty ptype_params))
               ty_decls)
      | _ -> acc)
    ~init:false sig_items

