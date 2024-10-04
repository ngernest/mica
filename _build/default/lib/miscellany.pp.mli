Caml1999N032����            2lib/miscellany.mli����  )1  �  �  }�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����(ppx_mica��.<command-line>A@A�A@I@@��A@@�A@J@@@@�@@�������@�@@@�@@�@@@@�@@@�@��������&Ppxlib��2lib/miscellany.mliA@E�A@K@A��A@@�A@K@@��A@@�A@K@�����*ocaml.text���@@ ���@@ �A�������0 {1 Miscellany} ��CMM�CMb@@��CMM�CMb@@@@��!CMM�"CMb@@��$CMM�%CMb@��'CMM�(CMb@���Р&printf��0F D H�1F D N@��@����&format��:F D q�;F D w@���!a��AF D R�BF D T@@@�������%Stdio+Out_channel!t��NF D V�OF D i@@��QF D V�RF D i@@@�����$unit��ZF D k�[F D o@@��]F D k�^F D o@@@@��`F D Q�aF D w@@@��!a��fF D {�gF D }@@@��iF D Q�jF D }@@@@���)ocaml.doc��@@ ��@@ �A�������: Alias for [Stdio.printf] ��{Edd�|Ed C@@��~Edd�Ed C@@@@���Edd��Ed C@@���Edd��Ed C@@���F D D��F D }@���F D D��F D }@���Р(with_loc���I � ���I � �@��@��!a���I � ���I � �@@@���#loc�����(Location!t���I � ���I � �@@���I � ���I � �@@@�����(Location#loc���I � ���I � �@���!a���I � ���I � �@@@@���I � ���I � �@@@���I � ���I � �@@@���I � ���I � �@@@@���]��z@@ ��{@@ �A�������	B Constructs a [loc] given some payload [txt] and a location [loc] ���H  ��H  �@@���H  ��H  �@@@@���H  ��H  �@@���H  ��H  �@@���I � ���I � �@���I � ���I � �@���Р&no_loc���L=A��L=G@��@�����(Location#loc���L=M��L=Y@���!a��L=J�L=L@@@@��L=J�L=Y@@@��!a��L=]�L=_@@@��L=J�L=_@@@@�������@@ ���@@ �A�������	8 Strips the location info from a value of type ['a loc] ��K � �� K �<@@��"K � ��#K �<@@@@��%K � ��&K �<@@��(K � ��)K �<@@��+L==�,L=_@��.L==�/L=_@���Р'max_loc��7P���8P��@��@�����(Location!t��CP���DP��@@��FP���GP��@@@��@�����(Location!t��RP���SP��@@��UP���VP��@@@�����(Location!t��_P���`P��@@��bP���cP��@@@��eP���fP��@@@��hP���iP��@@@@������@@ ��@@ �A�������	\ Maps a function [f] over a value of type ['a loc], 
    returning a value of type ['b loc] ��yNaa�zO��@@��|Naa�}O��@@@@��Naa��O��@@���Naa��O��@@���P����P��@���P����P��@���Р,map_with_loc���Tfj��Tfv@���!f��@��!a���Tf|��Tf~@@@��!b���Tf���Tf�@@@���Tf|��Tf�@@@��@�����(Location#loc���Tf���Tf�@���!a���Tf���Tf�@@@@���Tf���Tf�@@@�����(Location#loc���Tf���Tf�@���!b���Tf���Tf�@@@@���Tf���Tf�@@@���Tf���Tf�@@@���Tfy��Tf�@@@@���m���@@ ���@@ �A�������	g Takes the maximum of two [Location.t] values using Ppxlib's in-built 
    [Location.compare] function ���R����SCe@@���R����SCe@@@@���R����SCe@@���R����SCe@@���Tff��Tf�@���Tff��Tf�@���Р$map2���W��� W��@���!f��@��!a��W���W��@@@��!b��W���W��@@@��W���W��@@@��@�����!a��W��� W��@@@���!a��&W���'W�@@@@��)W���*W�@@@�����!b��2W��3W�@@@���!b��9W�
�:W�@@@@��<W��=W�@@@��?W���@W�@@@��BW���CW�@@@@���ٰ��@@ ���@@ �A�������	, Maps a function component-wise over a pair ��SV���TV��@@��VV���WV��@@@@��YV���ZV��@@��\V���]V��@@��_W���`W�@��bW���cW�@���Р)list_map4��k[���l[��@���!f��@��!a��w\���x\��@@@��@��!b��\����\��@@@��@��!c���\����\��@@@��@��!d���\����\��@@@��!e���\����\��@@@���\����\��@@@���\����\��@@@���\����\��@@@���\����\��@@@��@����$list���]����]��@���!a���]����]��@@@@���]����]��@@@��@����$list���^����^��@���!b���^����^��@@@@���^����^��@@@��@����$list���_����_��@���!c���_����_��@@@@���_����_��@@@��@����$list���`����`� @���!d���`����`��@@@@���`����`� @@@����$list���a	��a@���!e�� a�a@@@@��a�a@@@��`���a@@@��	_���
a@@@��^���a@@@��]���a@@@��\���a@@@@�������@@ ���@@ �A�������	� Maps a function [f] over 4 lists that must have the same length 
    - Raises [Invalid_argument] if the 4 lists have different lengths ��#Y�$ZS�@@��&Y�'ZS�@@@@��)Y�*ZS�@@��,Y�-ZS�@@��/[���0a@��2[���3a@���Р.tuple4_to_pair��;d37�<d3E@��@�����!a��Fd3H�Gd3J@@@���!b��Md3M�Nd3O@@@���!c��Td3R�Ud3T@@@���!d��[d3W�\d3Y@@@@��^d3H�_d3Y@@@�����!a��gd3]�hd3_@@@���!b��nd3b�od3d@@@@��qd3]�rd3d@@@��td3H�ud3d@@@@�����(@@ ��)@@ �A�������> Converts a 4-tuple to a pair ���c��c2@@���c��c2@@@@���c��c2@@���c��c2@@���d33��d3d@���d33��d3d@���Р-list_is_empty���i����i�@��@����$list���i�	��i�@���!a���i���i�@@@@���i���i�@@@����$bool���i���i�@@���i���i�@@@���i���i�@@@@���V��s@@ ��t@@ �A�������	� Checks if a list is empty
    - Backwards-compatible version of [List.is_empty], 
    which is only available in OCaml 5.1 and newer ���fff��h��@@���fff��h��@@@@���fff��h��@@���fff��h��@@���i����i�@���i����i�@���Р'list_or���o����o��@��@����$list���o����o��@�����$bool���o����o��@@���o����o��@@@@��o���o��@@@����$bool��	o���
o��@@��o���o��@@@��o���o��@@@@�������@@ ���@@ �A�������	� Takes the disjunction of a Boolean list
    - The empty list corresponds to false
    - Reimplementation of the [or] function in 
      Haskell's [GHC.Prelude] �� k�!n��@@��#k�$n��@@@@��&k�'n��@@��)k�*n��@@��,o���-o��@��/o���0o��@���Р+remove_last��8r�9r*@��@����$list��Br0�Cr4@���!a��Ir-�Jr/@@@@��Lr-�Mr4@@@����$list��Tr;�Ur?@���!a��[r8�\r:@@@@��^r8�_r?@@@��ar-�br?@@@@������@@ ��@@ �A�������	6 Retrieves all elements of a list except the last one ��rq���sq�@@��uq���vq�@@@@��xq���yq�@@��{q���|q�@@��~r�r?@���r��r?@���Р(get_last���v����v��@��@����$list���v����v��@���!a���v����v��@@@@���v����v��@@@��!a���v����v��@@@���v����v��@@@@���>��[@@ ��\@@ �A�������	e Returns the final element of a list (if one exists) 
    - Raises an exception if the list is empty ���tAA��uz�@@���tAA��uz�@@@@���tAA��uz�@@���tAA��uz�@@���v����v��@���v����v��@���Р:merge_list_with_assoc_list���~LP��~Lj@��@����$list���mr��mv@���!a���mo��mq@@@@���mo��mv@@@��@����$list���m���m�@������!b���m{��m}@@@���!c���m�� m�@@@@��m{�m�@@@@��mz�m�@@@���"eq��@��!a��m��m�@@@��@��!b��m��m�@@@����$bool��!m��"m�@@��$m��%m�@@@��'m��(m�@@@��*m��+m�@@@����$list��2m��3m�@������!a��<m��=m�@@@���!c��Cm��Dm�@@@@��Fm��Gm�@@@@��Im��Jm�@@@��Lm��Mm�@@@��Omz�Pm�@@@��Rmo�Sm�@@@@�����@@ ��@@ �A�������
  | [merge_list_with_assoc_list xs yzs ~eq] takes [xs : 'a list] 
    and an association list [yzs : ('b * 'c) list], and creates a 
    new association list of type [('a * 'c) list], using the function [eq] 
    to equate values of type ['a] and ['b] together
    - Raises an exception if there does not exist any element in [xs]
      that [eq] deems to be equal to a key in [yzs] ��cx���d}K@@��fx���g}K@@@@��ix���j}K@@��lx���m}K@@��o~LL�pm�@��r~LL�sm�@���Р1invert_assoc_list��{ E	�	��| E	�	�@��@����$list��� E	�	��� E	�	�@������!a��� E	�	��� E	�	�@@@���!b��� E	�	��� E	�	�@@@@��� E	�	��� E	�	�@@@@��� E	�	��� E	�	�@@@����$list��� E	�	��� E	�	�@������!b��� E	�	��� E	�	�@@@���!a��� E	�	��� E	�	�@@@@��� E	�	��� E	�	�@@@@��� E	�	��� E	�	�@@@��� E	�	��� E	�	�@@@@���U��r@@ ��s@@ �A�������	� Swaps the keys & values of an association list.
    - Note: bijectivity is not guaranteed since keys may appear more than once
    in the input association list.
    - Adapted from Jane street's [Base.List.Assoc.inverse] function ��� A���� D	[	�@@��� A���� D	[	�@@@@��� A���� D	[	�@@��� A���� D	[	�@@��� E	�	��� E	�	�@��� E	�	��� E	�	�@���Р7is_abs_ty_parameterized��� I
Y
]�� I
Y
t@��@����)signature��� I
Y
w�� I
Y
�@@��� I
Y
w�� I
Y
�@@@����$bool��� I
Y
��� I
Y
�@@��� I
Y
��  I
Y
�@@@�� I
Y
w� I
Y
�@@@@�������@@ ���@@ �A�������	w Returns true the abstract type declaration in a [signature] 
    is parameterized (e.g. ['a t]), else returns [false] �� G	�	�� H

X@@�� G	�	�� H

X@@@@�� G	�	�� H

X@@�� G	�	�� H

X@@�� I
Y
Y�  I
Y
�@��" I
Y
Y�# I
Y
�@@