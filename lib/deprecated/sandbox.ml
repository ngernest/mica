open! Ppxlib
open! Ast_helper
open! Asttypes

let item = {
  pstr_desc = Pstr_module
   {pmb_name = {txt = Some "Hello"};
    pmb_expr =
     {pmod_desc =
       Pmod_structure
        [{pstr_desc =
           Pstr_value (Nonrecursive,
            [{pvb_pat =
               {ppat_desc = Ppat_var {txt = "_message"};
                ppat_loc_stack = []};
              pvb_expr =
               {pexp_desc = Pexp_constant (Pconst_string ("Hello", ...));
                pexp_loc_stack = []}}])}]}}}