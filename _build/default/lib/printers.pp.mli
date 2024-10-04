Caml1999N032����            0lib/printers.mli����  ~    N  \�����1ocaml.ppx.context��&_none_@@ �A����������)tool_name���*ppx_driver@@@����,include_dirs����"[]@@@����)load_path!����
%@%@@����,open_modules*����.@.@@����+for_package3����$None8@8@@����%debug=����%falseB@B@@����+use_threadsG����
K@K@@����-use_vmthreadsP����T@T@@����/recursive_typesY����]@]@@����)principalb����%f@f@@����3transparent_modulesk����.o@o@@����-unboxed_typest����7x@x@@����-unsafe_string}����@�@�@@����'cookies�����"::�����������,library-name�@�@@����(ppx_mica��.<command-line>A@A�A@I@@��A@@�A@J@@@@�@@�������@�@@@�@@�@@@@�@@@�@��������&Ppxlib��0lib/printers.mliA@E�A@K@A��A@@�A@K@@��A@@�A@K@�����*ocaml.text���@@ ���@@ �A�������5 {1 Pretty-printers} ��CMM�CMg@@��CMM�CMg@@@@��!CMM�"CMg@@��$CMM�%CMg@��'CMM�(CMg@���Р*base_types��0I � ��1I �@���#loc�����(Location!t��>I ��?I �@@��AI ��BI �@@@����$list��II �#�JI �'@�����)core_type��RI ��SI �"@@��UI ��VI �"@@@@��XI ��YI �'@@@��[I ��\I �'@@@@���)ocaml.doc��@@ ��@@ �A�������	� List of OCaml base types 
    - The named argument [loc] is necessary in order for 
    the [Ppxlib.Metaquot] quotations to expand to the appropriate 
    AST fragments representing the base types. ��mEii�nH � �@@��pEii�qH � �@@@@��sEii�tH � �@@��vEii�wH � �@@��yI � ��zI �'@��|I � ��}I �'@���Р*pp_pattern���LNR��LN\@��@����'pattern���LN_��LNf@@���LN_��LNf@@@����$unit���LNj��LNn@@���LNj��LNn@@@���LN_��LNn@@@@���E��T@@ ��U@@ �A�������? Pretty-printer for [pattern]s ���K))��K)M@@���K))��K)M@@@@���K))��K)M@@���K))��K)M@@���LNN��LNn@���LNN��LNn@���Р,pp_core_type���O����O��@��@����)core_type���O����O��@@���O����O��@@@����$unit���O����O��@@���O����O��@@@���O����O��@@@@�������@@ ���@@ �A�������	! Pretty-printer for [core_type]s ���Npp��Np�@@���Npp��Np�@@@@���Npp��Np�@@���Npp��Np�@@��O���O��@��O���O��@���Р-pp_expression��R���R��@��@����*expression��R���R�@@��R���R�@@@����$unit��"R��#R�@@��%R��&R�@@@��(R���)R�@@@@���Ͱ��@@ ���@@ �A�������	" Pretty-printer for [expression]s ��9Q���:Q��@@��<Q���=Q��@@@@��?Q���@Q��@@��BQ���CQ��@@��ER���FR�@��HR���IR�@���Р1pp_structure_item��QU9=�RU9N@��@����.structure_item��[U9Q�\U9_@@��^U9Q�_U9_@@@����$unit��fU9c�gU9g@@��iU9c�jU9g@@@��lU9Q�mU9g@@@@����� @@ ��!@@ �A�������	& Pretty-printer for [structure_item]s ��}T�~T8@@���T��T8@@@@���T��T8@@���T��T8@@���U99��U9g@���U99��U9g@���Р,monomorphize���_����_��@��@����)core_type���_����_��@@���_����_��@@@����)core_type���_����_�@@���_����_�@@@���_����_�@@@@���U��d@@ ��e@@ �A�������
  p Instantiates all type variables ['a] inside a type expression with [int] 
  by recursing over the structure of the type expression. 
  Base types are left unchanged. 
  Note: this function only recurses over type expressions when 
  they consist of:
  - Type constructor applications ([Ptyp_constr])
  - Tuples ([Ptyp_tuple])
  - Arrow/function types ([Ptyp_arrow]). ���Wii��^��@@���Wii��^��@@@@���Wii��^��@@���Wii��^��@@���_����_�@���_����_�@���Р:string_of_monomorphized_ty���i/3��i/M@��@����)core_type���i/P��i/Y@@���i/P��i/Y@@@����&string���i/]��i/c@@���i/]��i/c@@@���i/P��i/c@@@@�������@@ ���@@ �A�������
   Converts a type expression [ty] to its capitalized, camel-case 
    string representation (for use as a constructor in an algebraic data type) 
    - The type expression is monomorphized prior to computing its string
    representation (i.e. ['a] is instantiated to [int]).
    - Note: polymoprhic variants, objects, extensions/attributes are 
    not supported by this function.  
    - Note: this function is slightly different from [Ppxlib.string_of_core_type]
    due to its capitalization, camel-case & monomorphization functionalities. ��a

�h�.@@��a

�	h�.@@@@��a

�h�.@@��a

�h�.@@��i//�i/c@��i//�i/c@���Р4snake_case_type_name��m���m��@��@����)core_type��'m���(m��@@��*m���+m��@@@����&string��2m���3m��@@��5m���6m��@@@��8m���9m��@@@@���ݰ��@@ ���@@ �A�������	a Retrieves the name of a type as a snake-case string 
    - e.g. [int list] becomes ["int_list"] ��Ikee�Jl��@@��Lkee�Ml��@@@@��Okee�Pl��@@��Rkee�Sl��@@��Um���Vm��@��Xm���Ym��@@