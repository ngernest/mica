open Ppxlib
open StdLabels
open Printers
open Miscellany

(** {1 Equality of [Parsetree] types} *)

(** Checks if a [constructor_declaration] for the [ty] ADT and 
    (its corresponding) [core_type] are equal with respect to their string 
    representations using [string_of_core_ty].
    - e.g. this function returns [true] when [core_ty = bool]
    and [constructor_declaration = Bool]. *)
let equal_ty_cstr_core_type (ty_cstr : constructor_declaration)
  (core_ty : core_type) : bool =
  String.equal (string_of_core_ty core_ty) ty_cstr.pcd_name.txt

(** Checks two [Longident.t] values for equality *)
let equal_longident (l1 : Longident.t) (l2 : Longident.t) : bool =
  Longident.compare l1 l2 = 0

(** Checks two [Longident.t Location.loc] values for equality, 
    ignoring their location *)
let equal_longident_loc (l1 : Longident.t Location.loc)
  (l2 : Longident.t Location.loc) : bool =
  equal_longident (no_loc l1) (no_loc l2)

(** Checks two [core_type]s for equality, ignoring location *)
let rec equal_core_type (t1 : core_type) (t2 : core_type) : bool =
  equal_core_type_desc t1.ptyp_desc t2.ptyp_desc

(** Checks two [core_type_desc]s for equality, ignoring location.
      - Does not support objects, classes, polymorphic variants, 
        universally quantified types, packages or extension nodes *)
and equal_core_type_desc (t1 : core_type_desc) (t2 : core_type_desc) : bool =
  match (t1, t2) with
  | Ptyp_any, Ptyp_any -> true
  | Ptyp_var x, Ptyp_var y -> String.equal x y
  | Ptyp_arrow (_, t11, t12), Ptyp_arrow (_, t21, t22) ->
    equal_core_type t11 t21 && equal_core_type t12 t22
  | Ptyp_tuple xs, Ptyp_tuple ys -> equal_core_type_list xs ys
  | Ptyp_constr (cstr_loc1, args1), Ptyp_constr (cstr_loc2, args2) ->
    let cstr1, cstr2 = map2 ~f:no_loc (cstr_loc1, cstr_loc2) in
    equal_longident cstr1 cstr2 && equal_core_type_list args1 args2
  | Ptyp_alias (tau1, _), Ptyp_alias (tau2, _) -> equal_core_type tau1 tau2
  | Ptyp_object _, Ptyp_object _
  | Ptyp_class _, Ptyp_class _
  | Ptyp_variant _, Ptyp_variant _
  | Ptyp_poly _, Ptyp_poly _
  | Ptyp_package _, Ptyp_package _
  | Ptyp_extension _, Ptyp_extension _ ->
    failwith "equality not supported for these types"
  | _, _ -> false

(** Checks two [core_type list]s for equality, ignoring location *)
and equal_core_type_list (xs : core_type list) (ys : core_type list) : bool =
  List.equal ~eq:equal_core_type xs ys

(** Checks two [mutable_flag]s for equality *)
let equal_mutable_flag x y =
  match (x, y) with
  | Mutable, Mutable | Immutable, Immutable -> true
  | _ -> false

(** Checks two [label_declaration]s for equality, ignoring location *)
let equal_label_declaration (l1 : label_declaration) (l2 : label_declaration) :
  bool =
  let name1, name2 = map2 ~f:no_loc (l1.pld_name, l2.pld_name) in
  String.equal name1 name2
  && equal_core_type l1.pld_type l2.pld_type
  && equal_mutable_flag l1.pld_mutable l2.pld_mutable

(** Checks two [constructor_argument]s for equality, ignoring location info *)
let equal_constructor_arguments (xs : constructor_arguments)
  (ys : constructor_arguments) : bool =
  match (xs, ys) with
  | Pcstr_tuple xss, Pcstr_tuple yss -> equal_core_type_list xss yss
  | Pcstr_record xss, Pcstr_record yss ->
    List.equal ~eq:equal_label_declaration xss yss
  | _ -> false

(** Checks two [constructor_declaration]s for equality, ignoring location *)
let rec equal_constructor_declaration (c1 : constructor_declaration)
  (c2 : constructor_declaration) : bool =
  let name1, name2 = map2 ~f:no_loc (c1.pcd_name, c2.pcd_name) in
  let vars1, vars2 = map2 ~f:(List.map ~f:no_loc) (c1.pcd_vars, c2.pcd_vars) in
  String.equal name1 name2
  && List.equal ~eq:String.equal vars1 vars2
  && equal_constructor_arguments c1.pcd_args c2.pcd_args
