(* open! Base
open! Ppxlib
open! Ast_helper 
open Ast_builder.Default
open Parsetree

(* Taken from [override] *)
let get_signature (modtype : Parsetree.module_type)
    : Parsetree.signature option =
  match modtype.pmty_desc with
  | Pmty_signature signature -> Some signature
  | _ -> None

let signature_impl (ld : label_declaration) = failwith "TODO"

let generate_impl ~ctxt (_rec_flag, type_declarations) = 
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations 
    ~f:(fun (td : type_declaration) -> 
        match td with 
        | { ptype_kind = Ptype_record _ | Ptype_variant _ | Ptype_open;
            ptype_loc;
            _; } ->
             let ext =
               Location.error_extensionf ~loc:ptype_loc
                 "Cannot derive for non module types"
             in
             [ Ast_builder.Default.pstr_extension ~loc ext [] ]
        | { ptype_kind = Ptype_abstract; _ } -> 
          List.map ~f:signature_impl signature
          
      )




let rec expr_of_type typ =
  let loc = typ.ptyp_loc in
  match typ with
  | [%type: int] -> [%expr fun x -> x + 1]
  | _ ->
      Location.raise_errorf ~loc "No support for this type: %s"
        (string_of_core_type typ)


(* Code generator *)
(* let generate_impl ~ctxt (_rec_flag, type_decls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map
    (fun typ_decl ->
      match typ_decl with
      | { ptype_kind = Ptype_abstract; ptype_manifest; _ } -> (
          match ptype_manifest with
          | Some t ->
              let addone = expr_of_type t in
              let func_name =
                if typ_decl.ptype_name.txt = "t" then { loc; txt = "addone" }
                else { loc; txt = typ_decl.ptype_name.txt ^ "_addone" }
              in
              [%stri let [%p Pat.var func_name] = [%e addone]]
          | None ->
              Location.raise_errorf ~loc "Cannot derive anything for this type"
          )
      | _ -> Location.raise_errorf ~loc "Cannot derive anything for this type")
    type_decls *)


let impl_generator = Deriving.Generator.V2.make_noarg generate_impl

let addone = Deriving.add "addone" ~str_type_decl:impl_generator     *)