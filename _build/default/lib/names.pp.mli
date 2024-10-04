Caml1999N032����            -lib/names.mli����  �  �  @  �����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����(ppx_mica��.<command-line>A@A�A@I@@��A@@�A@J@@@@�@@�������@�@@@�@@�@@@@�@@@�@��������&Ppxlib��-lib/names.mliA@E�A@K@A��A@@�A@K@@��A@@�A@K@�����*ocaml.text���@@ ���@@ �A�������> {1 Quoting & mangling names} ��D ^ ^�D ^ �@@��D ^ ^�D ^ �@@@@��!D ^ ^�"D ^ �@@��$D ^ ^�%D ^ �@��'D ^ ^�(D ^ �@���Р)add_prime��0G � ��1G � �@��@����&string��:G � ��;G � �@@��=G � ��>G � �@@@����&string��EG � ��FG � �@@��HG � ��IG � �@@@��KG � ��LG � �@@@@���)ocaml.doc�� @@ ��@@ �A�������	" Turns the variable [x] into [x'] ��]F � ��^F � �@@��`F � ��aF � �@@@@��cF � ��dF � �@@��fF � ��gF � �@@��iG � ��jG � �@��lG � ��mG � �@���Р5update_expr_arg_names��uKfj�vKf@��@����$list��Kf���Kf�@�����&string���Kf���Kf�@@���Kf���Kf�@@@@���Kf���Kf�@@@��@����$list���Kf���Kf�@�����&string���Kf���Kf�@@���Kf���Kf�@@@@���Kf���Kf�@@@����$list���Kf���Kf�@�����&string���Kf���Kf�@@���Kf���Kf�@@@@���Kf���Kf�@@@���Kf���Kf�@@@���Kf���Kf�@@@@���y��x@@ ��y@@ �A�������	� [update_expr_arg_names expr_args args] replaces each variable [x] in 
    [expr_args] if [x'] (the variable with a prime added) is in [expr_args] ���I � ���Je@@���I � ���Je@@@@���I � ���Je@@���I � ���Je@@���Kff��Kf�@���Kff��Kf�@�����ݰ��@@ ���@@ �A�������	! {1 Producing fresh identifiers} ���N����N�$@@���N����N�$@@@@���N����N�$@@�� N���N�$@��N���N�$@���Р(mk_fresh��U���U��@���#loc�����(Location!t��V���V��@@��V���V��@@@���!f���#loc�����(Location!t��/V���0V��@@��2V���3V��@@@��@����&string��<V���=V��@@��?V���@V��@@@��!a��EV���FV��@@@��HV���IV��@@@��KV���LV��@@@��@����)core_type��UV���VV��@@��XV���YV��@@@��!a��^V���_V��@@@��aV���bV��@@@��dV���eV��@@@��gV���hV��@@@@�����@@ ��@@ �A�������
  Z Produces a fresh identifier of type at location [loc], with the type [ty]
    of the variable serialized & prefixed to the resultant variable name 
    - Note that the type of the resultant identifier is polymorphic:
      The function argument [f] specifies how to take a [Location.t]
       and a [string] to form the desired identifier type. ��xP&&�yTH�@@��{P&&�|TH�@@@@��~P&&�TH�@@���P&&��TH�@@���U����V��@���U����V��@���Р-mk_fresh_pvar���Zlp��Zl}@���#loc�����(Location!t���Zl���Zl�@@���Zl���Zl�@@@��@����)core_type���Zl���Zl�@@���Zl���Zl�@@@����'pattern���Zl���Zl�@@���Zl���Zl�@@@���Zl���Zl�@@@���Zl���Zl�@@@@���t��s@@ ��t@@ �A�������	� Makes a fresh identifier of type [pattern] (a [Ppat_var]) for type [ty]
  - [mk_fresh_pvar] is [mk_fresh], specialized to [pattern]s ���X����Y,k@@���X����Y,k@@@@���X����Y,k@@���X����Y,k@@���Zll��Zl�@���Zll��Zl�@���Р-mk_fresh_evar���^>B��^>O@���#loc�����(Location!t���^>V��^>`@@���^>V��^>`@@@��@����)core_type��^>d�^>m@@��^>d�^>m@@@����*expression��^>q�^>{@@��^>q�^>{@@@��^>d�^>{@@@��^>R�^>{@@@@���̰��@@ ���@@ �A�������	� Makes a fresh identifier of type [expression] (a [Pexp_ident]) for type [ty]
    - [mk_fresh_pvar] is [mk_fresh], specialized to [expression]s ��(\���)]�=@@��+\���,]�=@@@@��.\���/]�=@@��1\���2]�=@@��4^>>�5^>{@��7^>>�8^>{@���Р5varnames_of_cstr_args��@dy}�Ady�@���#loc�����(Location!t��Ne���Oe��@@��Qe���Re��@@@��@����5constructor_arguments��[f���\f��@@��^f���_f��@@@���!f���#loc�����(Location!t��pg���qg��@@��sg���tg��@@@��@����&string��}g���~g��@@���g����g��@@@��!a���g����g��@@@���g����g��@@@���g����g��@@@����$list���h����h��@���!a���h����h��@@@@���h����h��@@@���g����h��@@@���f����h��@@@���e����h��@@@@���\��[@@ ��\@@ �A�������	� Produces fresh identifiers for [args] at [loc] 
    - Like [mk_fresh], the resultant identifier type is polymorphic: 
      The function argument [f] specifies how to take a [Location.t]
      and a [string] to form the desired identifier type. ���`}}��c<x@@���`}}��c<x@@@@���`}}��c<x@@���`}}��c<x@@���dyy��h��@���dyy��h��@���Р2pvars_of_cstr_args���m����m��@���#loc�����(Location!t���m����m��@@���m����m��@@@��@����5constructor_arguments���m����m��@@���m����m��@@@����$list���m� ��m�@�����'pattern���m��� m��@@��m���m��@@@@��m���m�@@@��m���	m�@@@��m���m�@@@@�������@@ ���@@ �A�������	� Takes [constructor_arguments] and produces a list of fresh identifiers 
    which each have type [pattern] 
    - This function is [varnames_of_cstr_args], specialized to [pattern]s ��j���lg�@@��j��� lg�@@@@��"j���#lg�@@��%j���&lg�@@��(m���)m�@��+m���,m�@���Р2evars_of_cstr_args��4r���5r��@���#loc�����(Location!t��Bs���Cs��@@��Es���Fs��@@@��@����5constructor_arguments��Os���Ps�	@@��Rs���Ss�	@@@����$list��Zs�	�[s�	@�����*expression��cs�	�ds�	@@��fs�	�gs�	@@@@��is�	�js�	@@@��ls���ms�	@@@��os���ps�	@@@@���$��#@@ ��$@@ �A�������	� Takes [constructor_arguments] and produces a list of fresh identifiers 
    which each have type [expression] 
    - This function is [varnames_of_cstr_args], specialized to [expression]s ���o��qy�@@���o��qy�@@@@���o��qy�@@���o��qy�@@���r����s�	@���r����s�	@��������E@@ ��F@@ �A�������	2 {1 Convering between different identifier types} ���v	q	q��v	q	�@@���v	q	q��v	q	�@@@@���v	q	q��v	q	�@@���v	q	q��v	q	�@���v	q	q��v	q	�@���Р;ppat_construct_of_cstr_decl���z

��z

!@���#loc�����(Location!t���{
$
*��{
$
4@@���{
$
*��{
$
4@@@��@����7constructor_declaration���{
$
8��{
$
O@@���{
$
8��{
$
O@@@����'pattern���{
$
S��{
$
Z@@���{
$
S��{
$
Z@@@���{
$
8��{
$
Z@@@���{
$
&��{
$
Z@@@@�������@@ ���@@ �A�������	R Takes a [constructor_declaration] and produces the pattern 
    [Ppat_construct] ���x	�	���y	�
@@���x	�	���y	�
@@@@���x	�	���y	�
@@�� x	�	��y	�
@@��z

�{
$
Z@��z

�{
$
Z@@