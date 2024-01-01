open! Ppxlib 
open Ast_helper

module List = ListLabels


let entrypoint : unit = 
  print_endline "mica_ppx"


let generate_expr ~ctxt (_rec_flag, tds) : structure_item list = 
  List.concat_map tds ~f:(fun td -> 
    let loc = Expansion_context.Deriver.derived_item_loc ctxt in
      begin match td with 
      | { ptype_kind = Ptype_abstract; ptype_name; _ } -> 
        let type_declaration = Ast_builder.Default.type_declaration 
          ~loc 
          ~name: { txt = "expr"; loc} 
          ~cstrs: []
          ~params: []
          ~kind: (Ptype_variant [{
            pcd_name = {txt = "T"; loc};
            pcd_vars = [];
            pcd_args = Pcstr_tuple [];
            pcd_res = None;
            pcd_loc = loc;
            pcd_attributes = []
          }])
          ~private_: Public 
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
  

let expr_generator = 
  Deriving.Generator.V2.make_noarg generate_expr


let deriver : Deriving.t = 
  Deriving.add "expr" ~str_type_decl:expr_generator 