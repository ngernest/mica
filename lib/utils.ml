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

(** Constructs a [module_expr] from a string containing the module name *)
let module_expr_of_string ~(loc : Location.t) (str : string) : module_expr =
  pmod_ident ~loc (with_loc ~loc (Longident.parse str))

(** [let_open ~loc M e] creates the expression 
    [let open M in e], where [m] is some [module_expr] *)
let let_open ~(loc : Location.t) (m : module_expr) (e : expression) : expression
    =
  let mod_infos = open_infos ~loc ~expr:m ~override:Fresh in
  pexp_open ~loc mod_infos e

(** [let_open_twice ~loc M1 M2 e] produces the [expression] 
    [let open M1 in let open M2 in e] at location [loc],
    for [module_expr]s [M1] and [M2] *)
let let_open_twice ~(loc : Location.t) (m1 : module_expr) (m2 : module_expr)
  (e : expression) =
  let_open ~loc m1 (let_open ~loc m2 e)

(******************************************************************************)
(** {1 Utility functions for working with Ppxlib} *)

(** [get_type_varams td] extracts the type parameters 
    from the type declaration [td]
    - Type variables (e.g. ['a]) are instantiated with [int] *)
let get_type_params (td : type_declaration) : core_type list =
  List.map td.ptype_params ~f:(fun (core_ty, _) -> monomorphize core_ty)

(** Turns a variable name [x] into [x'] *)
let rec add_prime (x : string) : string = x ^ "\'"

(******************************************************************************)

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

(** [get_match_arm ~loc expr_vars ~abs_ty_parameterized] returns the 
    match arms of the inner pattern match in [interp], e.g. 
    an expression of the form [ValIntT e]
    - The argument [expr_vars] is a list of variable names that 
    have type [expr]
    - The named argument [abs_ty_parameterized] represents whether the 
    abstract type [t] in the module signature is parameterized (e.g. ['a t]) *)
let get_match_arm (expr_vars : string list) ~(abs_ty_parameterized : bool)
  ~(loc : Location.t) : pattern =
  match expr_vars with
  | [] -> failwith "impossible: get_match_arm"
  | [ x ] -> mk_valt_pat ~loc ~abs_ty_parameterized (add_prime x)
  | _ ->
    let val_exprs : pattern list =
      List.map
        ~f:(fun x -> mk_valt_pat ~loc ~abs_ty_parameterized (add_prime x))
        expr_vars in
    ppat_tuple ~loc val_exprs

(** Creates the RHS of the inner case-stmt in [interp], for the special 
    case where we are dealing with a unary [value] constructor
    and a unary module function, e.g. [match e with ValInt x -> M.f x] 
    (In this example, [get_unary_case_rhs] produces the expression [M.f x])
    - [value_cstr] is the name of the constructor for the [value] type 
    - [expr_cstr] is the constructor for the [expr] type, which corresponds
    to a function inside the module with name [mod_name] 
    - [x] is the argument that will be applied to the module function *)
let get_unary_case_rhs (value_cstr : Longident.t Location.loc)
  (mod_name : string) (expr_cstr : Longident.t Location.loc) (x : string)
  ~(loc : Location.t) : expression =
  let mod_func = pexp_ident ~loc (add_lident_loc_prefix mod_name expr_cstr) in
  let mod_func_arg = pexp_ident_of_string (add_prime x) ~loc in
  let mod_func_app = [%expr [%e mod_func] [%e mod_func_arg]] in
  pexp_construct ~loc value_cstr (Some mod_func_app)

(** Variant of [get_unary_case_rhs] which handles the situation 
    when the RHS of the case statement is an n-ary function with 
    arguments [xs] *)
let get_nary_case_rhs (ret_ty_cstr : constructor_declaration)
  (mod_name : string) (expr_cstr : Longident.t Location.loc)
  (xs : expression list) ~loc : expression =
  let mod_func = pexp_ident ~loc (add_lident_loc_prefix mod_name expr_cstr) in
  let mod_func_app =
    pexp_apply ~loc mod_func (List.map ~f:(fun x -> (Nolabel, x)) xs) in
  let value_cstr = get_cstr_name ret_ty_cstr in
  pexp_construct ~loc value_cstr (Some mod_func_app)

(** [update_expr_arg_names expr_args args] replaces each variable [x] in 
    [expr_args] if [x'] (the variable with a prime added) is in [expr_args] *)
let update_expr_arg_names (expr_args : string list) (args : string list) :
  string list =
  List.map args ~f:(fun x ->
      if List.mem (add_prime x) ~set:expr_args then add_prime x else x)

(** Takes a [type_declaration] and returns a pair of the form 
    [(<type_name, list_of_type_parameters)] *)
let get_ty_name_and_params ({ ptype_name; ptype_params; _ } : type_declaration)
  : string * core_type list =
  let ty_params = List.map ~f:fst ptype_params in
  (ptype_name.txt, ty_params)

(** Takes a module signature and returns a list containing pairs of the form
    [(<type_name>, <list_of_type_parameters>)]. The list is ordered based on
    the order of appearance of the type declarations in the signature.  *)
let get_ty_decls_from_sig (sig_items : signature) :
  (string * core_type list) list =
  List.fold_left sig_items ~init:[] ~f:(fun acc { psig_desc; _ } ->
      match psig_desc with
      | Psig_type (_, ty_decls) ->
        List.map ~f:get_ty_name_and_params ty_decls :: acc
      | _ -> acc)
  |> List.concat |> List.rev

(* -------------------------------------------------------------------------- *)
(*      Helpers for deriving monadic code (currently unused)                  *)
(* -------------------------------------------------------------------------- *)

let monadic_bindop ~(loc : Location.t) (x : string) (exp : expression) :
  binding_op =
  let op = with_loc ~loc "bind" in
  let pat = ppat_var ~loc (with_loc ~loc x) in
  binding_op ~loc ~op ~pat ~exp

let let_monadic_bind ~(loc : Location.t) (x : string) (e1 : expression)
  (e2 : expression) : expression =
  let letop = letop ~let_:(monadic_bindop ~loc x e1) ~ands:[] ~body:e2 in
  pexp_letop ~loc letop
