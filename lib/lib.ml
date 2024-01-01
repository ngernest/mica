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
    match core_ty.ptyp_desc with 
    | Ptyp_var _ -> [%type: int]    
    | _ -> core_ty)

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
      | { ptype_kind = Ptype_abstract; ptype_name; _ } ->
        (* Constructor name = name of abstract type, capitalized *)
        let cstr_name = String.capitalize_ascii ptype_name.txt in 
        (* Generate a type declaration [type expr = T] *)
        let type_declaration = type_declaration 
          ~loc 
          ~name: { txt = "expr"; loc }   (* Name of type *)
          ~cstrs: []                     (* Type constraints, not needed here *)   
          ~params: []                    (* Type parameters *)
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
          (* [manifest] is the RHS of [type t =...], doesn't apply here *)
          ~manifest: None in             
        [{ pstr_loc = loc;
            pstr_desc = Pstr_type (_rec_flag, [type_declaration]) }]

      | { ptype_kind = _; ptype_loc; _ } -> 
         [ mkError ~local:ptype_loc ~global:loc 
           "Can't derive for non-abstract type" ]
      end)

(** [mk_constructor ~name ~loc arg_tys] creates a constructor with the [name] 
    for an algebraic data type at the location [loc] with 
    argument types [arg_tys] *)      
let mk_constructor ~(name : string) ~(loc : location) 
  (arg_tys : core_type list) : constructor_declaration = 
  { (* Constructor name *)
    pcd_name = { txt = name; loc };    
    (* Type variables *)
    pcd_vars = [];                 
    (* Constructor arguments *)
    pcd_args = Pcstr_tuple arg_tys;
    (* Constructor result *)
    pcd_res = None;              
    (* Location of the type *)  
    pcd_loc = loc;               
    (* Any PPXes attached to the type *)
    pcd_attributes = []          
  }     

(** Walks over all the [val ...] declarations in a module signature
    and creates the corresponding definition of the [expr] ADT *)  
let mk_expr_constructors (sig_items : signature_item list) : constructor_declaration list = 
  List.fold_left sig_items ~init:[] 
    ~f:(fun acc {psig_desc; _} -> 
      begin match psig_desc with 
      | Psig_type (rec_flag, type_decls) -> 
        (* Walk over all the type declarations in a signature 
          and create a corresponding constructor
          - TODO: maybe this is not needed since [expr] doesn't depend on ['a t] *)
        acc @ List.fold_left type_decls ~init:[] ~f:(fun innerAcc td -> 
          begin match td with 
          | { ptype_kind = Ptype_abstract; ptype_name; ptype_loc; _ } -> 
            (* Constructor name = name of abstract type, capitalized *)
            let name = String.capitalize_ascii ptype_name.txt in  
            mk_constructor ~name ~loc:ptype_loc (get_type_params td) :: innerAcc
          | _ -> failwith "TODO: maybe throw an exception here & let the caller catch it?"
          end)
      | Psig_value { pval_name; pval_type; _} -> 
          failwith "TODO: handle values" 
      | _ -> failwith "TODO: not sure how to handle other kinds of [signature_item_desc]"
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
        | { psig_desc = Psig_type (rec_flag, type_decls); _ } :: tl -> 
          (* TODO: figure out how to recurse over the tail [tl]
             by calling [mk_expr_constructors] somehow *)
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