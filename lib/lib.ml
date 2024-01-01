open Ppxlib 
open Ast_helper
open Ast_builder.Default

module List = ListLabels

(******************************************************************************)
(** {1 Utility functions} *)

(** Temp dummy definition for testing Dune *)
let entrypoint : unit = 
  print_endline "mica_ppx"  

(** [get_type_varams td] extracts the type parameters 
    from the type declaration [td]
    - Type variables (e.g. ['a]) are instantiated with [int] *)  
let get_type_params (td : type_declaration) : core_type list = 
  List.map td.ptype_params ~f:(fun (core_ty, _) ->
    let loc = core_ty.ptyp_loc in  
    begin match core_ty.ptyp_desc with 
    | Ptyp_var _ -> [%type: int]    
    | _ -> core_ty
    end)

(** [mkError ~local ~global msg] creates an error extension node, 
    associated with an element in the AST at the location [local],
    and reports the error message [msg] at the location [global] *)      
let mkError ~(local : location) ~(global : location) msg : structure_item = 
  let ext = Location.error_extensionf ~loc:local msg in 
  pstr_extension ~loc:global ext []        

(******************************************************************************)
(** {1 Core PPX functionality} *)  
  
(** [generate_expr ctxt (_rec_flag, tds)] takes [tds],
    a list of [type_declarations], and generates 
    the definition of the [expr] algebraic data type. 
    - [_rec_flag] is a flag indicating whether the expressions
    generated are recursive, which is required by Ppxlib but currently unused *)
let generate_expr ~ctxt (_rec_flag, tds) : structure_item list = 
  List.concat_map tds ~f:(fun td -> 
    (* Obtain the current location *)
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      begin match td with 
      | { ptype_kind = Ptype_abstract; ptype_name; ptype_params; _ } ->
        (* Constructor name = name of abstract type, capitalized *)
        let cstr_name = String.capitalize_ascii ptype_name.txt in 
        (* Generate a type declaration [type expr = T] *)
        let type_declaration = type_declaration 
          ~loc 
          ~name: { txt = "expr"; loc }     (* Name of type *)
          ~cstrs: []            
          ~params: []                     (* Type parameters *)
          ~kind: (Ptype_variant [{
            (* Constructor name *)
            pcd_name = { txt = cstr_name; loc };    
            (* Type variables *)
            pcd_vars = [];                 
            (* Constructor arguments *)
            pcd_args = Pcstr_tuple (get_type_params td);    
            (* Constructor result *)
            pcd_res = None;              
            (* Location of the type *)  
            pcd_loc = loc;               
            (* Any PPXes attached to the type *)
            pcd_attributes = []          
          }])
          ~private_: Public 
          (* [manifest] refers to the RHS of [type t = ...], 
             which doesn't apply here *)
          ~manifest: None in             
          [{ pstr_loc = loc;
             pstr_desc = Pstr_type (_rec_flag, [type_declaration]) }]

      | { ptype_kind = _; ptype_loc; _ } -> 
          let ext = Location.error_extensionf ~loc:ptype_loc 
            "Can't derive for non-abstract type" in 
          [ Ast_builder.Default.pstr_extension ~loc ext [] ]
      end)

 

(** Walks over a module signature definition and extracts the 
    abstract type declaration, producing the definition 
    of an ADT [expr] with one constructor sharing the 
    same name *)
let generate_expr_from_sig ~(ctxt : Expansion_context.Deriver.t)
    (mt : module_type_declaration) : structure_item list =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in 
  begin match mt with 
  | { pmtd_type = Some mod_type; 
      pmtd_name; pmtd_loc; _ } -> 
      begin match mod_type with 
      | { pmty_desc = Pmty_signature sig_items; pmty_loc; _ } -> 
        begin match sig_items with 
        | [] -> [ mkError ~local:pmtd_loc ~global:loc 
                  "Module signature can't be empty" ]
        | { psig_desc = Psig_type (rec_flag, type_decls); psig_loc } :: tl -> 
          generate_expr ~ctxt (rec_flag, type_decls)
        | { psig_desc; psig_loc } :: tl -> 
          failwith "TODO: handle psig_desc"
        end
      | _ -> failwith "TODO: other case for mod_type"
      end
  | { pmtd_type = None; pmtd_loc; pmtd_name; _} -> 
    [ mkError ~local:pmtd_loc ~global:loc 
      "Can't derive for expressions that aren't module type declarations" ]
  end       


(** Instantiates the PPX deriver *)  
let module_expr_generator : 
  (structure_item list, module_type_declaration) Deriving.Generator.t = 
  Deriving.Generator.V2.make_noarg generate_expr_from_sig 

  
(** Registered PPX deriver *)
let mod_deriver : Deriving.t = 
  (* Call [Deriving.add] to register the deriver.
     The [str_module_type_decl] indicates that the [[@@deriving ...]]
     syntax extension is to be added after [module type] declarations. *)
  Deriving.add "mica" ~str_module_type_decl:module_expr_generator      