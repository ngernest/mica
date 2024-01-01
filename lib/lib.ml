open Ppxlib 
open Ast_helper

module List = ListLabels

(** Temp dummy definition for testing Dune *)
let entrypoint : unit = 
  print_endline "mica_ppx"
  
(** [generate_expr ctxt (_rec_flag, tds)] takes [tds],
    a list of [type_declarations], and generates 
    the definition of the [expr] algebraic data type *)
let generate_expr ~ctxt (_rec_flag, tds) : structure_item list = 
  List.concat_map tds ~f:(fun td -> 
    (* Obtain the current location *)
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      begin match td with 
      | { ptype_kind = Ptype_abstract; ptype_name; _ } ->
        (* Constructor name = name of abstract type, capitalized *)
        let cstr_name = String.capitalize_ascii ptype_name.txt in 
        (* Generate a type declaration [type expr = T] *)
        let type_declaration = Ast_builder.Default.type_declaration 
          ~loc 
          ~name: { txt = "expr"; loc }     (* Name of type *)
          ~cstrs: []            
          ~params: []
          ~kind: (Ptype_variant [{
            pcd_name = { txt = cstr_name; loc };    
            pcd_vars = [];                 (* Type variables *)
            pcd_args = Pcstr_tuple [];     (* Constructor arguments *)
            pcd_res = None;                (* Constructor result *)
            pcd_loc = loc;               (* Location of the type *)
            pcd_attributes = []          (* Any PPXes attached to the type *)
          }])
          ~private_: Public 
          (* [manifest] refers to the RHS of [type t = ...], 
             which doesn't apply here *)
          ~manifest: None in             
          [{
            pstr_loc = loc;
            pstr_desc = Pstr_type (_rec_flag, [type_declaration])
          }]

      | { ptype_kind = _; ptype_loc; _ } -> 
          let ext = Location.error_extensionf ~loc:ptype_loc 
            "Can't derive for non-abstract type" in 
          [ Ast_builder.Default.pstr_extension ~loc ext [] ]
      end)

(** [mkError ~local ~global msg] creates an error extension node, 
    associated with an element in the AST at the location [local],
    and reports the error message [msg] at the location [global] *)      
let mkError ~(local : location) ~(global : location) msg : structure_item = 
  let ext = Location.error_extensionf ~loc:local msg in 
  Ast_builder.Default.pstr_extension ~loc:global ext []       

(** Walks over a module signature definition and extracts the 
    abstract type declaration, producing the definition 
    of an ADT [expr] with one constructor sharing the 
    same name *)
let generate_expr_from_sig 
  ~(ctxt : Expansion_context.Deriver.t)
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
  Deriving.add "mod_expr" ~str_module_type_decl:module_expr_generator      