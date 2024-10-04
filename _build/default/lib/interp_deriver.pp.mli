Caml1999N032����            6lib/interp_deriver.mli����  �  �  �  �����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����(ppx_mica��.<command-line>A@A�A@I@@��A@@�A@J@@@@�@@�������@�@@@�@@�@@@@�@@@�@��������&Ppxlib��6lib/interp_deriver.mliA@E�A@K@A��A@@�A@K@@��A@@�A@K@������%Utils��BLQ�BLV@A��BLL�BLV@@��BLL�BLV@���A�  # �6interp_case_rhs_params��!DX]�"DXs@@@��Р#loc��)Evz�*Ev}@@�����(Location!t��3Ev @�4Ev J@@��6Ev @�7Ev J@@@��9Evz�:Ev K@@�Р(mod_name��@F L P�AF L X@@����&string��HF L [�IF L a@@��KF L [�LF L a@@@��NF L P�OF L b@@�Р4abs_ty_parameterized��UG c g�VG c {@@����$bool��]G c ~�^G c �@@��`G c ~�aG c �@@@��cG c g�dG c �@@�Р)expr_cstr��jH � ��kH � �@@�����(Location#loc��tH � ��uH � �@������)Longident!t��H � ���H � �@@���H � ���H � �@@@@���H � ���H � �@@@���H � ���H � �@@�Р$args���I � ���I � �@@����&option���I � ���I � �@�����'pattern���I � ���I � �@@���I � ���I � �@@@@���I � ���I � �@@@���I � ���I � �@@�Р%gamma���J � ���J � �@@����'inv_ctx���J � ���J � �@@���J � ���J � �@@@���J � ���J � �@@�Р&ret_ty���K � ���K � �@@����)core_type���K � ���K � �@@���K � ���K � �@@@���K � ���K � �@@@A@@���DXX��L � �@@���DXX��L � �@���Р2mk_interp_case_rhs���Q����Q��@��@����6interp_case_rhs_params���Q����Q��@@���Q����Q��@@@����*expression���Q����Q��@@���Q����Q��@@@���Q����Q��@@@@���)ocaml.doc���@@ ���@@ �A�������	� Creates the body of the inner case-statement inside [interp]
  - NB: [gamma] is the "inverse typing context" which maps types 
    to variable names ��N � ��P}�@@��N � ��P}�@@@@��N � ��P}�@@��N � ��P}�@@��Q���Q��@��Q���Q��@���Р)mk_interp��'W���(W��@���#loc����(location��3X���4X��@@��6X���7X��@@@���4abs_ty_parameterized����$bool��BY��CY�@@��EY��FY�@@@��@����$list��OZW�PZ[@���������(Location#loc��]Z"�^Z.@������)Longident!t��hZ�iZ!@@��kZ�lZ!@@@@��nZ�oZ.@@@�����&option��wZ9�xZ?@�����'pattern���Z1��Z8@@���Z1��Z8@@@@���Z1��Z?@@@�����'inv_ctx���ZB��ZI@@���ZB��ZI@@@�����)core_type���ZL��ZU@@���ZL��ZU@@@@���Z��ZU@@@@���Z��Z[@@@����.structure_item���[_a��[_o@@���[_a��[_o@@@���Z��[_o@@@���Y����[_o@@@���X����[_o@@@@������l@@ ��m@@ �A�������	� Creates the definition for the [interp] function 
    (contained inside the body of the [Interpret] functor) 
    - The argument [expr_cstrs] is a list containing the 
    names & arg types of the constructors for the [expr] algebraic data type ���S����V��@@���S����V��@@@@���S����V��@@���S����V��@@���W����[_o@���W����[_o@���Р*mk_functor���^����^��@���#loc����(location���_����_��@@���_����_��@@@��@�����(Location#loc���`����`��@�����&option��`���`��@�����&string��`���`��@@��`���`��@@@@��`���`��@@@@��`���`��@@@��@����+module_type��!a���"a��@@��$a���%a��@@@��@����)signature��.b���/b�@@��1b���2b�@@@��@����$list��;cK�<cO@���������(Location#loc��Ic�Jc"@������)Longident!t��Tc
�Uc@@��Wc
�Xc@@@@��Zc
�[c"@@@�����&option��cc-�dc3@�����'pattern��lc%�mc,@@��oc%�pc,@@@@��rc%�sc3@@@�����'inv_ctx��{c6�|c=@@��~c6�c=@@@�����)core_type���c@��cI@@���c@��cI@@@@���c
��cI@@@@���c	��cO@@@����+module_expr���dSU��dS`@@���dSU��dS`@@@���c	��dS`@@@���b����dS`@@@���a����dS`@@@���`����dS`@@@���_����dS`@@@@������^@@ ��_@@ �A�������	- Creates the body of the [Interpret] functor ���]qq��]q�@@���]qq��]q�@@@@���]qq��]q�@@���]qq��]q�@@���^����dS`@���^����dS`@���Р0generate_functor���h����h��@���$ctxt������1Expansion_context'Deriver!t���i����i��@@���i����i��@@@��@����7module_type_declaration���i���i�@@���i���i�@@@����.structure_item���i���i�*@@���i���i�*@@@��i��i�*@@@��i���i�*@@@@������@@ ���@@ �A�������	\ Generates the scaffolding for the [Interpret] functor 
    (e.g. module type declarations) ��fbb�g��@@��fbb�g��@@@@��fbb�g��@@��fbb�g��@@��!h���"i�*@��$h���%i�*@@