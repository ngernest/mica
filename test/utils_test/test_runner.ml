open Alcotest
include All_tests

(******************************************************************************)
(* Overall Alcotest Test Suite *)

(** Array containing command-line arguments to [Alcotest]. 
    From the Alcotest docs:
    {e this array must have at least one element, and the first element will be 
      treated as if it was the command name and thus ignored for the 
      purposes of option processing.}
*)
let argv = [| "ignored"; "--compact" |]

let () =
  run "Utils test suite" ~argv
    [ ( "monomorphize (base types)",
        [ mono_int (); mono_string (); mono_bool () ] );
      ( "monomorphize (instantiate type variables)",
        [ mono_list ();
          mono_option ();
          mono_double_list ();
          mono_pair ();
          mono_pair_list ();
          mono_func_1_arg ();
          mono_func_2_args ();
          mono_poly_abs_type ();
          mono_qualified_poly_abs_type ()
        ] );
      ( "uniq_ret_tys",
        [ uniq_ret_tys_singleton ();
          uniq_ret_tys_no_dupes ();
          uniq_ret_tys_three_tys ();
          uniq_ret_ty_1_arg_funcs ();
          uniq_ret_ty_2_arg_funcs ()
        ] );
      ( "mk_ty_cstrs",
        [ mk_ty_cstrs_single_base_ty ();
          mk_ty_cstrs_single_mono_abs_ty ();
          mk_ty_cstrs_single_poly_abs_ty ();
          mk_ty_cstrs_two_base ();
          mk_ty_cstrs_no_dupes ()
        ] );
      ( "get_ret_ty",
        [ get_ret_ty_1_arg_func ();
          get_ret_ty_2_arg_func ();
          get_ret_ty_3_arg_func ();
          get_ret_ty_uncurried ()
        ] );
      ( "string_of_lident",
        [ string_of_lident_trivial ();
          string_of_lident_ldot ();
          string_of_lident_nested_lot ()
        ] );
      ( "uncapitalize_lident",
        [ uncapitalize_lident_trivial ();
          uncapitalize_lident_ldot ();
          uncapitalize_lident_ldot_nested ();
          uncapitalize_lident_ldot_doubly_nested ()
        ] );
      ( "add_lident_prefix",
        [ add_lident_prefix_mod_path (); add_lident_prefix_ldot () ] );
      ( "is_abs_ty_parameterized",
        [ is_abs_ty_parameterized_empty_sig ();
          is_abs_ty_parameterized_sig_no_abs_ty ();
          is_abs_ty_parameterized_t ();
          is_abs_ty_parameterized_alpha_t ();
          is_abs_ty_parameterized_alpha_beta_t ()
        ] );
      ( "update_expr_arg_names",
        [ update_expr_arg_names_singleton ();
          update_expr_arg_names_no_op ();
          update_expr_arg_names_update_one ();
          update_expr_arg_names_update_two ();
          update_expr_arg_names_double_primes ()
        ] );
      ( "get_ty_decls_from_sig",
        [ get_ty_decls_from_sig_t ();
          get_ty_decls_from_sig_t_int ();
          get_ty_decls_from_sig_alpha_t ();
          get_ty_decls_from_sig_alpha_beta_t ();
          get_ty_decls_from_sig_alpha_beta_gamma_t ();
          get_ty_decls_from_sig_ignore_vals_alpha_t ();
          get_ty_decls_from_sig_two_tys ();
          get_ty_decls_from_sig_three_tys ()
        ] );
      (* ( "get_arg_tys_of_expr_cstr", [ get_arg_tys_of_expr_cstr_hof ();
         get_arg_tys_of_expr_cstr_map () ] ); *)
      ( "equal_core_type_ty_cstr",
        [ equal_core_ty_ty_cstr_bool_Bool ();
          equal_core_ty_ty_cstr_int_Int ();
          equal_core_ty_ty_cstr_t_T ();
          equal_core_ty_ty_cstr_alpha_Int ();
          equal_core_ty_ty_cstr_alpha_t_IntT ();
          equal_core_ty_ty_cstr_alpha_int_list_IntList ();
          equal_core_ty_ty_cstr_alpha_string_option_StringOption ();
          equal_core_ty_ty_cstr_product_type ();
          equal_core_type_ty_cstr_function_type ()
        ] );
      ( "equal_core_type",
        [ equal_core_type_any_refl ();
          equal_core_type_int_refl ();
          equal_core_type_int_bool_neq ();
          equal_core_type_int_string_product_refl ();
          equal_core_type_int_string_product_permute ();
          equal_core_type_string_list_refl ();
          equal_core_type_different_list_types ();
          equal_core_type_nested_option_list_refl ();
          equal_core_type_function_types_refl ();
          equal_core_type_alpha_t_refl ();
          equal_core_type_alpha_beta_t_neq ()
        ] );
      ( "equal_constructor_declaration",
        [ equal_constructor_declaration_enum_refl ();
          equal_constructor_declaration_unary_refl ();
          equal_constructor_declaration_unary_diff_names ();
          equal_constructor_declaration_unary_same_name_diff_arg_types ();
          equal_constructor_declaration_binary_refl ();
          equal_constructor_declaration_binary_diff_names ();
          equal_constructor_declaration_binary_permute_args ()
        ] );
      ( "get_abs_ty_names",
        [ get_abs_ty_names_empty ();
          get_abs_ty_names_t ();
          get_abs_ty_names_alpha_t ();
          get_abs_ty_names_alpha_beta_t ();
          get_abs_ty_names_set_abstract_type ();
          get_abs_ty_names_two_types ();
          get_abs_ty_names_three_types ()
        ] );
      ( "check_type_is_concrete",
        [ check_type_is_concrete_t_int_true ();
          check_type_is_concrete_t_string_option_true ();
          check_type_is_concrete_t_int_bool_true ();
          check_type_is_concrete_t_t_false ();
          check_type_is_concrete_t_int_t_false ();
          check_type_is_concrete_t_alpha_t_false ();
          check_type_is_concrete_t_t_option_false ();
          check_type_is_concrete_t_int_t_list_false ();
          check_type_is_concrete_t_func_arg_false ();
          check_type_is_concrete_t_2nd_func_arg_false ();
          check_type_is_concrete_t_func_res_false ();
          check_type_is_concrete_t_prod_type_false ()
        ] );
      ( "get_cstr_arity",
        [ get_cstr_arity_nullary ();
          get_cstr_arity_unary ();
          get_cstr_arity_binary ();
          get_cstr_arity_ternary ()
        ] );
      ( "gen_atom",
        [ gen_atom_int ();
          gen_atom_char ();
          gen_atom_string ();
          gen_atom_int_list ();
          gen_atom_char_option ();
          gen_atom_tuple2 ();
          gen_atom_tuple3 ();
          gen_atom_option_list_pair ();
          gen_atom_nested_list ();
          gen_atom_expr_T ();
          gen_atom_expr_IntT ();
          gen_atom_t_IntT ();
          gen_atom_expr_IntIntT ();
          gen_atom_expr_IntIntIntT ()
        ] )
    ]
