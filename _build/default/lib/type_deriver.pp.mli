Caml1999N032����            4lib/type_deriver.mli����  ;�  �  #�  !�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����(ppx_mica��.<command-line>A@A�A@I@@��A@@�A@J@@@@�@@�������@�@@@�@@�@@@@�@@@�@��������&Ppxlib��4lib/type_deriver.mliA@E�A@K@A��A@@�A@K@@��A@@�A@K@������'Inv_ctx��BLQ�BLX@A��BLL�BLX@@��BLL�BLX@���Р-mk_expr_cstrs�� Jjn�!Jj{@��@����)signature��*Jj~�+Jj�@@��-Jj~�.Jj�@@@����$list��5Jj��6Jj�@��������7constructor_declaration��AJj��BJj�@@��DJj��EJj�@@@�����)core_type��MJj��NJj�@@��PJj��QJj�@@@@��SJj��TJj�@@@@��VJj��WJj�@@@��YJj~�ZJj�@@@@���)ocaml.doc��@@ ��@@ �A�������
  J Walks over all the [val ...] declarations in a module signature
    and creates the corresponding definition of the [expr] ADT 
    - The return type is a list of pairs, where each pair 
    consists of the declaration for the [expr] constructor, 
    along with the return type of the function (expressed as 
    a [core_type]) ��kDZZ�lITi@@��nDZZ�oITi@@@@��qDZZ�rITi@@��tDZZ�uITi@@��wJjj�xJj�@��zJjj�{Jj�@���Р,uniq_ret_tys���N ��N,@��@����)signature���N/��N8@@���N/��N8@@@����$list���NF��NJ@�����)core_type���N<��NE@@���N<��NE@@@@���N<��NJ@@@���N/��NJ@@@@���Q��^@@ ��_@@ �A�������	_ Extracts the unique return types of all [val] declarations within a 
        module signature ���L����M @@���L����M @@@@���L����M @@���L����M @@���N��NJ@���N��NJ@���Р+mk_cstr_aux���Ugk��Ugv@��@����)signature���Vy{��Vy�@@���Vy{��Vy�@@@���!f��@����)core_type���W����W��@@���W����W��@@@����7constructor_declaration���W����W��@@���W����W��@@@���W��� W��@@@����$list��X���X��@�����7constructor_declaration��X���X��@@��X���X��@@@@��X���X��@@@��W���X��@@@��Vy{�X��@@@@���ð��@@ ���@@ �A�������
   Helper function for creating the constructors of the [ty] and [value] 
    algebraic data types 
    - The argument [sig_items] contains the contents of a module signature
    - [~f] is a function that specifies how to turn a [core_type] into a 
    [constructor_declaration] ��-PLL�.TFf@@��0PLL�1TFf@@@@��3PLL�4TFf@@��6PLL�7TFf@@��9Ugg�:X��@��<Ugg�=X��@���Р+mk_ty_cstrs��E]w{�F]w�@��@����)signature��O]w��P]w�@@��R]w��S]w�@@@����$list��Z]w��[]w�@�����7constructor_declaration��c]w��d]w�@@��f]w��g]w�@@@@��i]w��j]w�@@@��l]w��m]w�@@@@����� @@ ��!@@ �A�������	� Constructs the definition of the [ty] algebraic data type
    based on the unique return types of all [val] declarations within 
    the module signature ��}Z���~\[v@@���Z����\[v@@@@���Z����\[v@@���Z����\[v@@���]ww��]w�@���]ww��]w�@���Р+mk_val_cstr���c����c��@��@����)core_type���c����c��@@���c����c��@@@����7constructor_declaration���c����c��@@���c����c��@@@���c����c��@@@@���W��d@@ ��e@@ �A�������	� [mk_val_cstr ty] constructors the corresponding constructor declaration
    for the [value] datatype, given some [core_type] [ty]
    - e.g. if [ty = Int], [mk_val_cstr] returns the declaration for 
      the [ValInt] constructor ���_����b�@@���_����b�@@@@���_����b�@@���_����b�@@���c����c��@���c����c��@���Р,mk_val_cstrs���gJN��gJZ@��@����)signature���gJ]��gJf@@���gJ]��gJf@@@����$list���gJ���gJ�@�����7constructor_declaration���gJj��gJ�@@���gJj��gJ�@@@@���gJj��gJ�@@@�� gJ]�gJ�@@@@�������@@ ���@@ �A�������	k Constructs the definition of the [value] algebraic data type
    based on the inhabitants of the [ty] ADT ��e���fI@@��e���fI@@@@��e���fI@@��e���fI@@��gJJ�gJ�@�� gJJ�!gJ�@���Р1mk_generator_name��)k���*k�@��@����&string��3k��4k�@@��6k��7k�@@@����&string��>k��?k�@@��Ak��Bk�@@@��Dk��Ek�@@@@������@@ ���@@ �A�������	_ Takes the name of a type and produces the name of its 
    corresponding QuickCheck generator ��Ui���Vj��@@��Xi���Yj��@@@@��[i���\j��@@��^i���_j��@@��ak���bk�@��dk���ek�@���Р(gen_atom��mp���np��@���#loc�����(Location!t��{q���|q��@@��~q���q��@@@��@����)core_type���r��r@@���r��r@@@���'abs_tys����$list���s6��s:@��������&string���s��s#@@���s��s#@@@�����$list���s0��s4@�����)core_type���s&��s/@@���s&��s/@@@@���s&��s4@@@@���s��s4@@@@���s��s:@@@����*expression���t>@��t>J@@���t>@��t>J@@@���s��t>J@@@���r��t>J@@@���q����t>J@@@@������@@ ���@@ �A�������	� Produces an atomic QuickCheck generator for the given [core_type]
    - [abs_tys] is an association list consisting of type names & type 
    parameters for the abstract types in the signature  ���m��o��@@���m��o��@@@@���m��o��@@���m��o��@@���p����t>J@���p����t>J@���Р4mint_generator_names��y���y�	@��@����$list��y�	-�y�	1@�����7constructor_declaration��y�	�y�	,@@��y�	�y�	,@@@@��y�	�y�	1@@@����$list��"y�	<�#y�	@@�����&string��+y�	5�,y�	;@@��.y�	5�/y�	;@@@@��1y�	5�2y�	@@@@��4y�	�5y�	@@@@@���۰��@@ ���@@ �A�������	� Produces the name of QuickCheck generators corresponding to a list of 
    [constructor_declaration]s (by prepending the prefix "gen" to each 
    constructor's name) ��EvLL�Fx��@@��HvLL�Ix��@@@@��KvLL�Lx��@@��NvLL�Ox��@@��Qy���Ry�	@@��Ty���Uy�	@@���Р,gen_expr_rhs��]~

�^~

 @���#loc�����(Location!t��k
#
)�l
#
3@@��n
#
)�o
#
3@@@��@����$list��x @
7
Q�y @
7
U@�����7constructor_declaration��� @
7
9�� @
7
P@@��� @
7
9�� @
7
P@@@@��� @
7
9�� @
7
U@@@���'abs_tys����$list��� A
Y
}�� A
Y
�@��������&string��� A
Y
d�� A
Y
j@@��� A
Y
d�� A
Y
j@@@�����$list��� A
Y
w�� A
Y
{@�����)core_type��� A
Y
m�� A
Y
v@@��� A
Y
m�� A
Y
v@@@@��� A
Y
m�� A
Y
{@@@@��� A
Y
d�� A
Y
{@@@@��� A
Y
c�� A
Y
�@@@����*expression��� B
�
��� B
�
�@@��� B
�
��� B
�
�@@@��� A
Y
[�� B
�
�@@@��� @
7
9�� B
�
�@@@���
#
%�� B
�
�@@@@���{���@@ ���@@ �A�������	� Helper function for producing the RHS of the pattern match in gen_expr 
    - [abs_tys] is an association list consisting of type names & type 
    parameters for the abstract types in the signature ���{	B	B��}	�
@@���{	B	B��}	�
@@@@���{	B	B��}	�
@@���{	B	B��}	�
@@���~

�� B
�
�@���~

�� B
�
�@���Р,is_base_case��� J���� J�@��@����7constructor_declaration�� J�� J�%@@��
 J�� J�%@@@����$bool�� J�)� J�-@@�� J�)� J�-@@@�� J�� J�-@@@@�������@@ ���@@ �A�������
  b Determines if an [expr] constructor can be used as a base case for 
    [gen_expr]. A constructor can be used as the base case if:
    - It is nullary
    - It has no arguments of type [expr] (i.e. the corresponding function
      in the signature has no arguments of type [t]) 
    - Note: constructors with record arguments are currently unsupported. ��) D
�
��* I��@@��, D
�
��- I��@@@@��/ D
�
��0 I��@@��2 D
�
��3 I��@@��5 J���6 J�-@��8 J���9 J�-@���Р6check_type_is_concrete��A U59�B U5O@��@����$list��K U5Y�L U5]@�����&string��T U5R�U U5X@@��W U5R�X U5X@@@@��Z U5R�[ U5]@@@��@����)core_type��d U5a�e U5j@@��g U5a�h U5j@@@����$bool��o U5n�p U5r@@��r U5n�s U5r@@@��u U5a�v U5r@@@��x U5R�y U5r@@@@�����,@@ ��-@@ �A�������
    [check_type_is_concrete abs_ty_names ty] determines whether [ty] is a concrete 
    type based on [abs_ty_names], a list containing the names of abstract types 
    in a signature. 
    
    For example, if a module signature defines an abstract type ['a t], 
    then [int t] would {i not} be concrete, but [int] and [bool] would be 
    considered concrete. 
    - Note: type variables (e.g. ['a]) are considered concrete by this function
    (since they're technically not defined inside a module signature) ��� L//�� T�4@@��� L//�� T�4@@@@��� L//�� T�4@@��� L//�� T�4@@��� U55�� U5r@��� U55�� U5r@���Р0mk_gen_expr_case��� d���� d��@��@����$list��� e���� e��@��������&string��� e���� e��@@��� e���� e��@@@�����$list��� e���� e��@�����)core_type��� e���� e��@@��� e���� e��@@@@��� e���� e��@@@@��� e���� e��@@@@��� e���� e��@@@���,is_base_case����$bool��� f���� f��@@��� f���� f��@@@��@����)core_type��� g���� g��@@��� g���� g��@@@��@����$list��� h���� h�@�����7constructor_declaration�� h��� h��@@��
 h��� h��@@@@�� h��� h�@@@����$case�� i� i@@�� i� i@@@�� h��� i@@@�� g��� i@@@��! f���" i@@@��$ e���% i@@@@���˰��@@ ���@@ �A�������
   [mk_gen_expr_case abs_tys ty rhs_cstrs] constructs a single case in the 
    pattern-match of the body of [gen_expr].
    - [abs_tys] is a list containing pairs of the form
    [(<type_name>, <list_of_type_parameters>)]. Most likely, this list is 
    obtained by calling [get_ty_decls_from_sig] in [getters.ml]. 
    - [ty] is the type we are matching on in the LHS of the pattern match 
    inside [gen_expr]
    - [rhs_cstrs] are the constructors for [expr] that have that type (to be 
    generated on the RHS of the pattern match).
    - [is_base_case] is an optional Boolean argument that indicates whether 
    the constructors in [rhs_cstrs] are base cases for [gen_expr] 
    (as determined by the [is_base_case] function). This parameter 
    defaults to [false]. ��5 Wtt�6 ce�@@��8 Wtt�9 ce�@@@@��; Wtt�< ce�@@��> Wtt�? ce�@@��A d���B i@��D d���E i@���Р.gen_expr_cases��M lBF�N lBT@��@����)signature��W lBW�X lB`@@��Z lBW�[ lB`@@@����$list��b lBi�c lBm@�����$case��k lBd�l lBh@@��n lBd�o lBh@@@@��q lBd�r lBm@@@��t lBW�u lBm@@@@�����(@@ ��)@@ �A�������	/ Creates the main case statement in [gen_expr] ��� k�� kA@@��� k�� kA@@@@��� k�� kA@@��� k�� kA@@��� lBB�� lBm@��� lBB�� lBm@���Р/derive_gen_expr��� p���� p��@���#loc�����(Location!t��� p���� p�@@��� p���� p�@@@��@����)signature��� p��� p�@@��� p��� p�@@@����*expression��� p��� p�@@��� p��� p�@@@��� p��� p�@@@��� p���� p�@@@@���s���@@ ���@@ �A�������	k Derives the [gen_expr] QuickCheck generator 
    - [ty_cstrs] is a list of constructors for the [ty] ADT  ��� noo�� o��@@��� noo�� o��@@@@��� noo�� o��@@��� noo�� o��@@��� p���� p�@��� p���� p�@���Р-deriving_show��� sim�� siz@���#loc�����(Location!t�� si�� si�@@�� si�� si�@@@����)attribute�� si�� si�@@�� si�� si�@@@�� si}� si�@@@@�������@@ ���@@ �A�������	B Produces the attribute [[@@deriving show { with_path = false }]] ��% r!!�& r!h@@��( r!!�) r!h@@@@��+ r!!�, r!h@@��. r!!�/ r!h@@��1 sii�2 si�@��4 sii�5 si�@���Р7generate_types_from_sig��= xBF�> xB]@���$ctxt������1Expansion_context'Deriver!t��M y`g�N y`�@@��P y`g�Q y`�@@@��@����7module_type_declaration��Z z���[ z��@@��] z���^ z��@@@����$list��e {���f {��@�����.structure_item��n {���o {��@@��q {���r {��@@@@��t {���u {��@@@��w z���x {��@@@��z y`b�{ {��@@@@���!��.@@ ��/@@ �A�������	� Walks over a module signature definition and extracts the 
    abstract type declaration, producing the definition 
    the [expr] and [ty] algebraic data types ��� u���� wA@@��� u���� wA@@@@��� u���� wA@@��� u���� wA@@��� xBB�� {��@��� xBB�� {��@���Р.get_expr_cstrs��� �qu�� �q�@��@����+module_type��� ����� ���@@��� ����� ���@@@����$list��� ����� ���@���������(Location#loc��� ����� ���@������)Longident!t��� ����� ���@@��� ����� ���@@@@��� ����� ���@@@�����&option��� ����� ���@�����'pattern��� ����� ���@@��� ����� ���@@@@��� ����� ���@@@�����'inv_ctx��� ����� ���@@��� ����� ���@@@�����)core_type�� ���� ���@@�� ���� ���@@@@��
 ���� ���@@@@�� ���� ���@@@�� ���� ���@@@@�������@@ ���@@ �A�������	� Helper function: given [mod_ty], a module signature,
    [get_expr_cstrs] produces [expr] constructor names & arguments
    that match the declarations in the module signature ��! }���" 6p@@��$ }���% 6p@@@@��' }���( 6p@@��* }���+ 6p@@��- �qq�. ���@��0 �qq�1 ���@@