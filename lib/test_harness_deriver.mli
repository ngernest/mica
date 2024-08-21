open Ppxlib

(** Produces a single pattern of the form [ValInt x1], 
    where [val_cstr_pat] is the name of the [value] constructor 
    and [arg] is the name of the constructor argument *)
val mk_val_cstr_app :
  loc:Location.t -> Longident.t Location.loc -> string -> Ppxlib.pattern

(** Produces a pattern of the form [(ValInt x1, ValInt x2)] - [x1, x2] are the
  names for the two vairables - [value_cstr] is the name of the constructor for
  the [value] type *)
val mk_val_cstr_app2 :
  loc:Location.t -> string -> string * string -> Ppxlib.pattern

(** Produces a fresh constructor argument with the name of the type 
    as a prefix *)
val mk_fresh_cstr_arg : Ppxlib.core_type -> string

(** Takes in a type and produces a [pattern] containing the name of a test 
    function for that type.
    - e.g. [test_function_name ~loc ty] returns [Ppat_var "test_ty"] *)  
val test_function_name : loc:Location.t -> core_type -> pattern  

(** Produces a test function (eg [test_int]), where:
   - [ty] is the concrete type at which observational equivalence is being tested
   - [ty_cstr] is the corresponding constructor in the [ty] datatype
   - [value_cstr] is the corresponding constructor of the [value] datatype 
   - [test_name] is the name of the test (eg [test_int] )*)
val produce_test :
  loc:Location.t ->
  Ppxlib.core_type ->
  string ->
  string ->
  Ppxlib.pattern ->
  Ppxlib.structure_item

(** [check_type_is_concrete abs_ty_names ty] determines whether [ty] is a concrete 
    type based on [abs_ty_names], a list containing the names of abstract types 
    in a signature *)  
val check_type_is_concrete : string list -> core_type -> bool   

(** Produces test functions for all the concrete return types of functions 
    exposed in the module signature [sig_items] *)  
val derive_test_functions :
  loc:Location.t -> Ppxlib.signature -> Ppxlib.structure_item list

(** Derives the [TestHarness] functor *)
val generate_functor :
  ctxt:Ppxlib.Expansion_context.Deriver.t ->
  Ppxlib.module_type_declaration ->
  Ppxlib.structure
