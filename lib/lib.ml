open Ppxlib 
open Ast_helper

module List = ListLabels

(** Temp dummy definition for testing Dune *)
let entrypoint : unit = 
  print_endline "mica_ppx"


let generate_expr ~ctxt (_rec_flag, tds) : structure_item list = 
  List.concat_map tds ~f:(fun td -> 
    (* Obtain the current location *)
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      begin match td with 
      | { ptype_kind = Ptype_abstract; ptype_name; _ } -> 
        (* Generate a type declaration [type expr = T] *)
        let type_declaration = Ast_builder.Default.type_declaration 
          ~loc 
          ~name: { txt = "expr"; loc }     (* Name of type *)
          ~cstrs: []            
          ~params: []
          ~kind: (Ptype_variant [{
            pcd_name = {txt = "T"; loc};    (* Constructor name *)
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

      | { ptype_kind = _;
          ptype_loc; _;
        } -> 
          let ext = Location.error_extensionf ~loc:ptype_loc 
            "Can't derive for non-abstract type" in 
          [ Ast_builder.Default.pstr_extension ~loc ext [] ]
      end)
  


let expr_generator : 
  (structure_item list, 
    rec_flag * type_declaration list) Deriving.Generator.t = 
  Deriving.Generator.V2.make_noarg generate_expr

(** Actual PPX deriver *)
let deriver : Deriving.t = 
  (* Call [Deriving.add] to register the deriver.
     The [str_type_decl] named argument specifies what to do 
     if we want to generate [.ml] files. *)
  Deriving.add "expr" ~str_type_decl:expr_generator 