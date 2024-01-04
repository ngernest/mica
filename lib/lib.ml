open Ppxlib 
open Ast_helper
open Ast_builder.Default
open StdLabels

open Utils

(******************************************************************************)
(** {1 Core PPX functionality} *)  
  
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
  | ty' when List.mem ty' ~set:(base_types ~loc) -> [ty']
  | { ptyp_desc = Ptyp_constr ({txt = Lident tyconstr; _}, _); _ } as ty' -> 
    if String.equal tyconstr !abstract_ty_name then 
      if is_arrow then [[%type: expr]] else []
    else [ty']
  | { ptyp_desc = Ptyp_arrow (_, t1, t2); _ } -> 
      get_constructor_arg_tys ~is_arrow:true t1 
        @ get_constructor_arg_tys ~is_arrow:true t2
  | { ptyp_desc = Ptyp_tuple tys; _ } -> 
      List.concat_map ~f:(get_constructor_arg_tys ~is_arrow) tys 

  | _ -> failwith "TODO: get_constructor_arg_tys"

(** Extracts the (monomorphized) return type of a type expression 
    (i.e. the rightmost type in an arrow type) *)  
let rec get_ret_ty (ty : core_type) : core_type = 
  let loc = ty.ptyp_loc in 
  let ty_mono = monomorphize ty in 
  if List.mem ty_mono ~set:(base_types ~loc) 
    then ty_mono
  else 
    match ty_mono.ptyp_desc with 
    | Ptyp_constr _ | Ptyp_tuple _ | Ptyp_any | Ptyp_var _ -> ty_mono
    | Ptyp_arrow (_, _, t2) -> get_ret_ty t2
    | _ -> failwith "Type expression not supported by get_ret_ty"


(** Walks over all the [val ...] declarations in a module signature
    and creates the corresponding definition of the [expr] ADT *)  
let mk_expr_constructors (sig_items : signature) : constructor_declaration list = 
  List.fold_left sig_items ~init:[] 
    ~f:(fun acc {psig_desc; psig_loc; _} -> 
      begin match psig_desc with 
      | Psig_type (rec_flag, type_decls) -> []
      | Psig_value { pval_name; pval_type; pval_loc; _} -> 
          let name = String.capitalize_ascii pval_name.txt in 
          (* Exclude the return type of the function from the list 
             of argument types for the [expr] data constructor *)
          let arg_tys = remove_last (get_constructor_arg_tys pval_type) in 
          mk_constructor ~name ~loc:pval_loc ~arg_tys :: acc
      | Psig_attribute attr -> failwith "TODO: handle attribute [@@@id]"
      | Psig_extension (ext, attrs) -> failwith "TODO: handle extensions"
      | _ -> failwith "TODO: not sure how to handle other kinds of [signature_item_desc]"
      end)


(** Extracts the unique return types of all [val] declarations within a 
    module signature *)  
let uniq_ret_tys (sig_items : signature) : core_type list = 
  List.fold_left sig_items ~init:[] ~f:(fun acc {psig_desc; psig_loc; _} -> 
    begin match psig_desc with 
    | Psig_value { pval_type; _} -> 
      let ty = get_ret_ty pval_type in 
      if List.mem ty ~set:acc then acc 
      else ty :: acc
    | _ -> acc
    end)

(** Constructs the definition of the [ty] algebraic data type
    based on the unique return types of all [val] declarations within 
    the module signature *)    
let mk_ty_constructors (sig_items : signature) : constructor_declaration list = 
  let ret_tys = uniq_ret_tys sig_items in 
  let uniq_ret_tys = List.sort_uniq ret_tys
    ~cmp:(fun t1 t2 -> String.compare (string_of_core_type t1) (string_of_core_type t2)) in 
  List.map uniq_ret_tys 
    ~f:(fun ty -> 
      mk_constructor 
      ~name:(string_of_core_type ty)
      ~loc:ty.ptyp_loc 
      ~arg_tys:[])       

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
        | [] -> [ mk_error ~local:pmtd_loc ~global:loc 
                  "Module signature can't be empty" ]
        | _ -> 
          let expr_td = mk_adt ~loc ~name:"expr"
            ~constructors:(List.rev (mk_expr_constructors sig_items)) in
          let ty_td = mk_adt ~loc ~name:"ty"
            ~constructors:(mk_ty_constructors sig_items) in 
          [ pstr_type ~loc Recursive [expr_td];
            pstr_type ~loc Recursive [ty_td] ] 
        end
      | _ -> failwith "TODO: other case for mod_type"
      end
  | { pmtd_type = None; pmtd_loc; pmtd_name; _} -> 
    [ mk_error ~local:pmtd_loc ~global:loc 
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
