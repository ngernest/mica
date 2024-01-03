open Ppxlib 
open Ast_helper
open Ast_builder.Default

open StdLabels

(******************************************************************************)
(** {1 Utility functions} *)

(** Temp dummy definition for testing Dune *)
let entrypoint : unit = 
  print_endline "mica_ppx"  

(** Retrieves all elements of a list except the last one *)  
let rec remove_last (lst : 'a list) : 'a list = 
  match lst with 
  | [] | [_] -> [] 
  | x :: xs -> x :: remove_last xs 

(** Instantiates all type variables ['a] inside a type with [int] *)  
let monomorphize (ty : core_type) : core_type = 
  let loc = ty.ptyp_loc in  
  match ty.ptyp_desc with 
  | Ptyp_var _ -> [%type: int]    
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

(******************************************************************************)
(** {1 Core PPX functionality} *)  
  
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

  
(** Takes [ty], the type of a [val] declaration in a signature,
    and returns the type of the arguments of the corresponding 
    constructor for the [expr] datatype. 

    For the [Set] module signature example,
    - [val empty : 'a t] corresponds to the 0-arity [Empty] constructor
    - [val is_empty : 'a t -> bool] corresponds to [Is_empty of expr * bool] 
    - Monormorphic primitive types are preserved. 

    The [is_arrow] optional 
    named argument specifies whether [ty] is an arrow type: if yes, then 
    references to abstract types should be replaced with [expr], otherwise
    an occurrence of an abstract type in an non-arrow type (e.g. [val empty : 'a t])
    should be ignored (so [val empty : 'a t] corresponds to the 0-arity constructor [Empty]).
*)  
let rec get_constructor_arg_tys ?(is_arrow = false) (ty : core_type) : core_type list = 
  let loc = ty.ptyp_loc in 
  match (monomorphize ty) with 
  | ([%type: int]   | [%type: int32]  | [%type: int64] | [%type: nativeint] |
     [%type: char]  | [%type: bool]   | [%type: unit] |
     [%type: float] | [%type: string] ) as ty' -> [ty']
  | { ptyp_desc = Ptyp_constr ({txt = Lident tyconstr; _}, _); _ } as ty' -> 
    if String.equal tyconstr !abstract_ty_name then 
      if is_arrow then [[%type: expr]] else []
    else [ty']
  | { ptyp_desc = Ptyp_arrow (_, t1, t2); _} -> 
      get_constructor_arg_tys ~is_arrow:true t1 
        @ get_constructor_arg_tys ~is_arrow:true t2

  | _ -> failwith "TODO: get_constructor_arg_tys"

(** Walks over all the [val ...] declarations in a module signature
    and creates the corresponding definition of the [expr] ADT *)  
let mk_expr_constructors (sig_items : signature_item list) : constructor_declaration list = 
  List.fold_left sig_items ~init:[] 
    ~f:(fun acc {psig_desc; psig_loc; _} -> 
      begin match psig_desc with 
      | Psig_type (rec_flag, type_decls) -> []
      | Psig_value { pval_name; pval_type; pval_loc; _} -> 
          let name = String.capitalize_ascii pval_name.txt in 
          (* Exclude the return type of the function from the list 
             of argument types for the [expr] data constructor *)
          let arg_tys = remove_last (get_constructor_arg_tys pval_type) in 
          mk_constructor ~name ~loc:pval_loc arg_tys :: acc
      | Psig_attribute attr -> failwith "TODO: handle attribute [@@@id]"
      | Psig_extension (ext, attrs) -> failwith "TODO: handle extensions"
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
        | _ -> 
         

          let td = type_declaration 
            ~loc 
            ~name: { txt = "expr"; loc }   (* Name of type *)
            ~cstrs: []                     (* Type constraints, not needed here *)   
            ~params: []                    (* Type parameters *)
            ~kind: (Ptype_variant (List.rev @@ mk_expr_constructors sig_items))
            ~private_: Public 
            (* [manifest] is the RHS of [type t =...], doesn't apply here *)
            ~manifest: None in  
          let attr = attribute ~loc 
            ~name:{txt = "sexp_of"; loc} 
            ~payload:(PTyp [%type: expr]) in 
          let td_with_attr = { td with ptype_attributes = [attr] } in 
          [{ pstr_loc = loc;
              pstr_desc = Pstr_type (Recursive, [td_with_attr]) }]
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