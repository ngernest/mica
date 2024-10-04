Caml1999N032����            /lib/getters.mli����  B�  �  '�  %�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����(ppx_mica��.<command-line>A@A�A@I@@��A@@�A@J@@@@�@@�������@�@@@�@@�@@@@�@@@�@��������&Ppxlib��/lib/getters.mliA@E�A@K@A��A@@�A@K@@��A@@�A@K@������'Inv_ctx��BLQ�BLX@A��BLL�BLX@@��BLL�BLX@�����*ocaml.text���@@ ���@@ �A�������	8 {1 Working with constructors for algebraic data types} ��+E k k�,E k �@@��.E k k�/E k �@@@@��1E k k�2E k �@@��4E k k�5E k �@��7E k k�8E k �@���Р+get_varname��@I15�AI1@@��@����'pattern��JI1C�KI1J@@��MI1C�NI1J@@@����&string��UI1N�VI1T@@��XI1N�YI1T@@@��[I1C�\I1T@@@@���)ocaml.doc��@@ ��@@ �A�������	� Extracts the variable name from a [Ppat_var] pattern 
  - Raises [Not_found] if the input pattern is not of the form [Ppat_var] ��mG � ��nH �0@@��pG � ��qH �0@@@@��sG � ��tH �0@@��vG � ��wH �0@@��yI11�zI1T@��|I11�}I1T@���Р8get_arg_tys_of_expr_cstr���] ��]8@���(is_arrow����$bool���^;G��^;K@@���^;G��^;K@@@��@����)core_type���^;O��^;X@@���^;O��^;X@@@��@����$list���^;c��^;g@�����&string���^;\��^;b@@���^;\��^;b@@@@���^;\��^;g@@@����$list���^;u��^;y@�����)core_type���^;k��^;t@@���^;k��^;t@@@@���^;k��^;y@@@���^;\��^;y@@@���^;O��^;y@@@���^;=��^;y@@@@������@@ ���@@ �A�������
  � Takes [ty], the type of a [val] declaration in a signature,
    and returns the type of the arguments of the corresponding 
    constructor for the [expr] datatype. 
    - The [abs_tys] argument is a list of abstract types
      that are defined in the module signature (to determine
      when arguments should be instantiated with the [expr] type)

    For the [Set] module signature example,
    - [val empty : 'a t] corresponds to the 0-arity [Empty] constructor
    - [val is_empty : 'a t -> bool] corresponds to [Is_empty of expr * bool] 
    - Monomorphic primitive types are preserved. 

    The [is_arrow] optional 
    named argument specifies whether [ty] is an arrow type: if yes, then 
    references to abstract types should be replaced with [expr], otherwise
    an occurrence of an abstract type in an non-arrow type 
    (e.g. [val empty : 'a t]) should be ignored (so [val empty : 'a t] 
    corresponds to the nullary constructor [Empty]). ���KVV��\�@@���KVV��\�@@@@���KVV��\�@@���KVV��\�@@���]��^;y@���]��^;y@���Р-get_cstr_args��h���h��@���#loc�����(Location!t��i���i��@@��i���i��@@@��@��@��!a��i���i��@@@����)core_type��&i���'i��@@��)i���*i��@@@��,i���-i��@@@��@����$list��6i���7i��@���!a��=i���>i��@@@@��@i���Ai��@@@�������'pattern��Ki���Li��@@��Ni���Oi��@@@�����'inv_ctx��Wi���Xi��@@��Zi���[i��@@@@��]i���^i��@@@��`i���ai��@@@��ci���di��@@@��fi���gi��@@@@�����@@ ��@@ �A�������
   Helper function: [get_cstr_args loc get_ty args] takes [args], 
  a list containing the {i representation} of constructor arguments, 
  applies the function [get_ty] to each element of [args] and produces 
  a formatted tuple of constructor arguments (using the [ppat_tuple] smart 
  constructor for the [pattern] type).  
  - Note that [args] has type ['a list], i.e. the representation of 
  constructor arguments is polymorphic -- this function is instantiated 
  with different types when called in [get_cstr_metadata] ��w`{{�xgP�@@��z`{{�{gP�@@@@��}`{{�~gP�@@���`{{��gP�@@���h����i��@���h����i��@���Р1get_cstr_metadata���n����n��@��@����$list���o����o��@��������7constructor_declaration���o����o��@@���o����o��@@@�����)core_type���o����o��@@���o����o��@@@@���o����o��@@@@���o����o��@@@����$list���p�	.��p�	2@���������(Location#loc���p����p�	@������)Longident!t���p����p��@@���p����p��@@@@���p����p�	@@@�����&option���p�	��p�	@�����'pattern���p�	��p�	@@���p�	��p�	@@@@���p�	��p�	@@@�����'inv_ctx��p�	�p�	 @@��p�	�p�	 @@@�����)core_type��p�	#�p�	,@@��p�	#�p�	,@@@@��p���p�	,@@@@��p���p�	2@@@��o���p�	2@@@@�������@@ ���@@ �A�������	� Takes a list of [constructor_declaration]'s and returns 
    a list consisting of 4-tuples of the form 
    (constructor name, constructor arguments, typing context, return type) ��+k���,mT�@@��.k���/mT�@@@@��1k���2mT�@@��4k���5mT�@@��7n���8p�	2@��:n���;p�	2@���Р9get_cstr_metadata_minimal��Ct	�	��Dt	�	�@��@����$list��Mu	�	��Nu	�	�@�����7constructor_declaration��Vu	�	��Wu	�	�@@��Yu	�	��Zu	�	�@@@@��\u	�	��]u	�	�@@@����$list��dv	�
$�ev	�
(@���������(Location#loc��rv	�
�sv	�
@������)Longident!t��}v	�	��~v	�
@@���v	�	���v	�
@@@@���v	�	���v	�
@@@�����&option���v	�
��v	�
"@�����'pattern���v	�
��v	�
@@���v	�
��v	�
@@@@���v	�
��v	�
"@@@@���v	�	���v	�
"@@@@���v	�	���v	�
(@@@���u	�	���v	�
(@@@@���I��X@@ ��Y@@ �A�������	z Variant of [get_cstr_metadata] which returns 
      only a list of pairs containing constructor names & constructor args ���r	4	4��s	f	�@@���r	4	4��s	f	�@@@@���r	4	4��s	f	�@@���r	4	4��s	f	�@@���t	�	���v	�
(@���t	�	���v	�
(@���Р-get_cstr_name���z
�
���z
�
�@��@����7constructor_declaration���z
�
���z
�
�@@���z
�
���z
�
�@@@�����(Location#loc���z
�
���z
�
�@������)Longident!t���z
�
���z
�
�@@���z
�
���z
�
�@@@@���z
�
���z
�
�@@@���z
�
���z
�
�@@@@�������@@ ���@@ �A�������	] Extracts the constructor name (along with its location) from 
    a constructor declaration ��	x
*
*�
y
l
�@@��x
*
*�y
l
�@@@@��x
*
*�y
l
�@@��x
*
*�y
l
�@@��z
�
��z
�
�@��z
�
��z
�
�@���Р4get_cstrs_of_ty_decl��! @���" @��@��@����0type_declaration��+ A���, A��@@��. A���/ A��@@@����$list��6 A��7 A�"@���������(Location#loc��D A���E A�@������)Longident!t��O A���P A��@@��R A���S A��@@@@��U A���V A�@@@�����&option��^ A��_ A�@�����'pattern��g A��h A�@@��j A��k A�@@@@��m A��n A�@@@@��p A���q A�@@@@��s A���t A�"@@@��v A���w A�"@@@@�����*@@ ��+@@ �A�������	� Takes a [type_declaration] for an algebraic data type 
    and returns a list of (constructor name, constructor arguments) 
    - Raises an exception if the [type_declaration] doesn't correspond to an 
      algebraic data type ���|
�
�����@@���|
�
�����@@@@���|
�
�����@@���|
�
�����@@��� @���� A�"@��� @���� A�"@���Р.get_cstr_arity��� Djn�� Dj|@��@����7constructor_declaration��� Dj�� Dj�@@��� Dj�� Dj�@@@����#int��� Dj��� Dj�@@��� Dj��� Dj�@@@��� Dj�� Dj�@@@@���_��n@@ ��o@@ �A�������	@ Computes the arity of a constructor for an algebraic data type ��� C$$�� C$i@@��� C$$�� C$i@@@@��� C$$�� C$i@@��� C$$�� C$i@@��� Djj�� Dj�@��� Djj�� Dj�@���Р0get_cstr_arg_tys��� G���� G�@��@����7constructor_declaration��� G��� G�@@��� G��� G�@@@����$list��� G�+�� G�/@�����)core_type�� G�!� G�*@@�� G�!� G�*@@@@�� G�!� G�/@@@��
 G�� G�/@@@@�������@@ ���@@ �A�������	J Retrieves the argument types of a constructor for an algebraic data type �� F��� F��@@�� F��� F��@@@@��! F���" F��@@��$ F���% F��@@��' G���( G�/@��* G���+ G�/@��������@@ ���@@ �A�������	6 {1 Working with type parameters & type declarations} ��= J���> J��@@��@ J���A J��@@@@��C J���D J��@@��F J���G J��@��I J���J J��@���Р/get_type_params��R OY]�S OYl@��@����0type_declaration��\ OYo�] OY@@��_ OYo�` OY@@@����$list��g OY��h OY�@�����)core_type��p OY��q OY�@@��s OY��t OY�@@@@��v OY��w OY�@@@��y OYo�z OY�@@@@�����-@@ ��.@@ �A�������	� [get_type_varams td] extracts the type parameters 
    from the type declaration [td]
    - Type variables (e.g. ['a]) are instantiated with [int] ��� L���� NX@@��� L���� NX@@@@��� L���� NX@@��� L���� NX@@��� OYY�� OY�@��� OYY�� OY�@���Р*get_ret_ty��� S�� S@��@����)core_type��� S�� S"@@��� S�� S"@@@����)core_type��� S&�� S/@@��� S&�� S/@@@��� S�� S/@@@@���b��q@@ ��r@@ �A�������	o Extracts the (monomorphized) return type of a type expression 
    (i.e. the rightmost type in an arrow type) ��� Q���� R�@@��� Q���� R�@@@@��� Q���� R�@@��� Q���� R�@@��� S�� S/@��� S�� S/@���Р6get_ty_name_and_params��� W���� W��@��@����0type_declaration��� W���� W��@@��� W���� W��@@@�������&string��� W���� W��@@�� W��� W��@@@�����$list��
 W��� W��@�����)core_type�� W��� W��@@�� W��� W��@@@@�� W��� W��@@@@�� W��� W��@@@�� W���  W��@@@@���İ��@@ ���@@ �A�������	h Takes a [type_declaration] and returns a pair of the form 
    [(<type_name, list_of_type_parameters)] ��0 U11�1 Vp�@@��3 U11�4 Vp�@@@@��6 U11�7 Vp�@@��9 U11�: Vp�@@��< W���= W��@��? W���@ W��@���Р5get_ty_decls_from_sig��H \���I \��@��@����)signature��R \���S \��@@��U \���V \��@@@����$list��] \��^ \�@��������&string��i \���j \��@@��l \���m \��@@@�����$list��u \��v \�@�����)core_type��~ \�� \�
@@��� \��� \�
@@@@��� \��� \�@@@@��� \���� \�@@@@��� \���� \�@@@��� \���� \�@@@@���2��A@@ ��B@@ �A�������	� Takes a module signature and returns a list containing pairs of the form
    [(<type_name>, <list_of_type_parameters>)]. The list is ordered based on
    the order of appearance of the type declarations in the signature.  ��� Y���� [��@@��� Y���� [��@@@@��� Y���� [��@@��� Y���� [��@@��� \���� \�@��� \���� \�@���Р4get_abs_tys_from_sig��� `vz�� `v�@��@����)signature��� `v��� `v�@@��� `v��� `v�@@@����$list��� `v��� `v�@�����0type_declaration��� `v��� `v�@@��� `v��� `v�@@@@��� `v��� `v�@@@��� `v��� `v�@@@@�������@@ ���@@ �A�������	Y Retrieves all the abstract types from a signature as a list of 
    [type_declaration]s ��� ^�� _[u@@��� ^�� _[u@@@@��� ^�� _[u@@��� ^�� _[u@@��� `vv�� `v�@��� `vv�� `v�@���Р0get_abs_ty_names�� c��� c�@��@����)signature�� c�� c�@@�� c�� c�@@@����$list�� c�$� c�(@�����&string��$ c��% c�#@@��' c��( c�#@@@@��* c��+ c�(@@@��- c��. c�(@@@@���Ұ��@@ ���@@ �A�������	> Retrieves the names of all the abstract types in a signature ��> b���? b��@@��A b���B b��@@@@��D b���E b��@@��G b���H b��@@��J c���K c�(@��M c���N c�(@�����6��@@ ��@@ �A�������	" {1 Working with pattern matches} ��` f{{�a f{�@@��c f{{�d f{�@@@@��f f{{�g f{�@@��i f{{�j f{�@��l f{{�m f{�@���Р-get_match_arm��u o?C�v o?P@��@����$list�� pS\�� pS`@�����&string��� pSU�� pS[@@��� pSU�� pS[@@@@��� pSU�� pS`@@@���4abs_ty_parameterized����$bool��� pSy�� pS}@@��� pSy�� pS}@@@���#loc�����(Location!t��� pS��� pS�@@��� pS��� pS�@@@����'pattern��� pS��� pS�@@��� pS��� pS�@@@��� pS��� pS�@@@��� pSd�� pS�@@@��� pSU�� pS�@@@@���g��v@@ ��w@@ �A�������
  � [get_match_arm ~loc expr_vars ~abs_ty_parameterized] returns the 
    match arms of the inner pattern match in [interp], e.g. 
    an expression of the form [ValIntT e]
    - The argument [expr_vars] is a list of variable names that 
    have type [expr]
    - The named argument [abs_ty_parameterized] represents whether the 
    abstract type [t] in the module signature is parameterized (e.g. ['a t]) ��� h���� n�>@@��� h���� n�>@@@@��� h���� n�>@@��� h���� n�>@@��� o??�� pS�@��� o??�� pS�@���Р2get_unary_case_rhs��� z���� z��@��@�����(Location#loc��� {���� {�@������)Longident!t�� {��� {��@@�� {��� {��@@@@�� {���	 {�@@@��@����&string�� |
� |
@@�� |
� |
@@@��@�����(Location#loc��! }$�" }0@������)Longident!t��, }�- }#@@��/ }�0 }#@@@@��2 }�3 }0@@@��@����&string��< ~46�= ~4<@@��? ~46�@ ~4<@@@���#loc�����(Location!t��M @F�N @P@@��P @F�Q @P@@@����*expression��X �TV�Y �T`@@��[ �TV�\ �T`@@@��^ @B�_ �T`@@@��a ~46�b �T`@@@��d }�e �T`@@@��g |
�h �T`@@@��j {���k �T`@@@@�����@@ ��@@ �A�������
  1 Creates the RHS of the inner pattern-match in [interp], for the special 
    case where we are dealing with a unary [value] constructor
    and a unary module function, e.g. [match e with ValInt x -> M.f x] 
    (In this example, [get_unary_case_rhs] produces the expression [M.f x])
    - [value_cstr] is the name of the constructor for the [value] type 
    - [expr_cstr] is the constructor for the [expr] type, which corresponds
    to a function inside the module with name [mod_name] 
    - [x] is the argument that will be applied to the module function ��{ r���| y��@@��~ r��� y��@@@@��� r���� y��@@��� r���� y��@@��� z���� �T`@��� z���� �T`@���Р1get_nary_case_rhs��� ����� ��@��@����7constructor_declaration��� ��� �,@@��� ��� �,@@@��@����&string��� �02�� �08@@��� �02�� �08@@@��@�����(Location#loc��� �<J�� �<V@������)Longident!t��� �<>�� �<I@@��� �<>�� �<I@@@@��� �<>�� �<V@@@��@����$list��� �Zg�� �Zk@�����*expression��� �Z\�� �Zf@@��� �Z\�� �Zf@@@@��� �Z\�� �Zk@@@���#loc�����(Location!t��� �ou�� �o@@��� �ou�� �o@@@����*expression��� ����� ���@@��� ����  ���@@@�� �oq� ���@@@�� �Z\� ���@@@�� �<>�	 ���@@@�� �02� ���@@@�� �� ���@@@@�������@@ ���@@ �A�������	� Variant of [get_unary_case_rhs] which handles the situation 
    when the RHS of the case statement is an n-ary function with 
    arguments [xs] �� �bb�  ���@@��" �bb�# ���@@@@��% �bb�& ���@@��( �bb�) ���@@��+ ����, ���@��. ����/ ���@@