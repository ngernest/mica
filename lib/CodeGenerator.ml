open! Base
open! PPrint
open! ParserTypes

(** Document representing modules that need to be imported *)
let imports : document = 
  (string "open! Base") 
  ^^ hardline 
  ^^ (string "open! Base_quickcheck")
  ^^ hardline

(** Document for printing the PPX annotation for S-Expr serialization (indented),
    followed by a newline *)
let sexpAnnotation : document = 
  blank 2 ^^ !^ "[@@deriving sexp]" ^^ hardline   

(** Document for the [expr] ADT definition which is generated
    from a module signature  *)
(* let exprADTDecl : document = 
  let open OCaml in 
  prefix 2 1 
  (!^ "type expr =")
  (variant "expr" "Empty" 1 []) *)

(** Extracts the argument types of functions defined in the module signature,
    and generates constructors for the [expr] ADT 
    that take these types as type parameters *)
let extractArgTypes (v : valDecl) : document = 
  let open String in 
  let constr = capitalize (valName v) in
  match valType v with 
  | Func1 (arg, _) -> 
    !^ constr
    ^^ (!^ " of ")
    ^^ (!^ (string_of_ty arg))
  | Func2 (arg1, arg2, _) -> 
    !^ constr
    ^^ !^ " of "
    ^^ !^ (string_of_ty ~alpha:"int" arg1)
    ^^ !^ " * "
    ^^ !^ (string_of_ty ~alpha:"int" arg2)
  | _ -> !^ constr

(** Generates the definition of the [expr] ADT *)  
let exprADTDecl (m : moduleSig) : document = 
  prefix 2 1 
  (!^ "type expr =")
  (group @@ separate_map (hardline ^^ !^ " | ") extractArgTypes m.valDecls 
    ^/^ sexpAnnotation)  

(** Helper function for printing out OCaml constructors
    (Wrapper for the [OCaml.variant] function in the [PPrint] library) *)    
let printConstructor (constr : string) (args : string list) : document = 
  match args with 
  | [] -> !^ constr
  | [arg] -> !^ constr ^^ blank 1 ^^ !^ arg
  | _ -> OCaml.variant "expr" constr 1 (List.map ~f:string args)

(** [varNameHelper ty] returns an appropriate variable name corresponding 
    to [ty], eg. [varNameHelper Int = n] *)  
let varNameHelper (ty : ty) : string = 
  match ty with 
  | Alpha -> "a"
  | T | AlphaT -> "e"
  | Int -> "n"
  | _ -> String.prefix (string_of_ty ty) 1

(** Takes a list of argument types, and generates corresponding variable names 
    which are unique for each element of the list 
    
    eg. [genVarNames [Int, Int] = [n1, n2]] *)
let genVarNames (argTys : ty list) : string list = 
  match argTys with 
  | [] -> []
  | [ty] -> [varNameHelper ty]
  | _ -> List.mapi ~f:(fun i ty -> varNameHelper ty ^ Int.to_string (i + 1)) 
    argTys
    

(** Fetches the constructor corresponding to a [val] 
    declaration in the [expr] ADT *)
let getExprConstructor (v : valDecl) : document = 
  let constr = String.capitalize (valName v) in
  match constr, valType v with 
  | "Empty", _ -> !^ constr
  | _, Int -> printConstructor constr ["n"]
  | _, Char -> printConstructor constr ["c"]
  | _, Bool -> printConstructor constr ["b"]
  | _, Unit -> OCaml.unit
  | _, Alpha -> printConstructor constr ["a"]
  | _, T | _, AlphaT -> printConstructor constr ["t"]
  | _, Func1 (arg, _) -> printConstructor constr (genVarNames [arg])
  | _, Func2 (arg1, arg2, _) -> 
    printConstructor constr (genVarNames [arg1; arg2])

(** Extracts the return type of a function 
    For non-arrow types, this function just extracts the type itself *)    
let extractReturnTypes (v : valDecl) : string = 
  match valType v with 
  | Func1 (_, ret) | Func2 (_, _, ret) -> (string_of_ty ~t:"T" ret)
  | ty -> (string_of_ty ~t:"T" ty)

(** Fetches the unique return types across the functions / values 
    in a module signature *)  
let uniqRetTypesInSig (m : moduleSig) : string list = 
  let open String in
  List.dedup_and_sort ~compare:compare
    @@ List.map ~f:(fun ty -> extractReturnTypes ty |> capitalize) m.valDecls

(** Generates the definition of the [ty] ADT *)  
let tyADTDecl (m : moduleSig) : document = 
  let retTypes = uniqRetTypesInSig m in 
  prefix 2 1
  (!^ "type ty =")
  (group @@ separate_map (!^ " | ") (!^) retTypes
    ^/^ sexpAnnotation
    ^^ hardline)

(** Helper function called by [valueADTDecl]: creates a constructor 
    for the [value] ADT corresponding to the supplied [ty] *)
let mkValADTConstructor (ty : string) : document = 
  let open String in 
  !^ ("Val" ^ capitalize ty) 
  ^^ (!^ " of ")
  ^^ (!^ (uncapitalize ty |> fun ty -> if ty = "t" then "M.t" else ty))

(** Generates the [value] ADT definition (enclosed within the [ExprToImpl] functor) *)  
let valueADTDecl (m : moduleSig) : document = 
  let valueTypes = uniqRetTypesInSig m in 
  prefix 2 1 
  (!^ "type value = ")
  (group @@ separate_map (!^ " | ") mkValADTConstructor valueTypes 
    ^/^ sexpAnnotation)  


(** TODO: find a way of generating the pattern-matches for [interp] *)

let interpDefn (m : moduleSig) : document = 
  hang 2 @@ 
  !^ "let rec interp (expr : expr) : value = " 
  ^/^ (!^ "match expr with")
  ^/^ (!^ " | ")
  ^^ separate_map (hardline ^^ !^ " | ") getExprConstructor m.valDecls


(** Generates the definition of the [ExprToImpl] functor *)  
let functorDef (m : moduleSig) ~(sigName : string) ~(functorName : string) : document = 
  hang 2 @@ 
  !^  (Printf.sprintf "module %s (M : %s) = struct " functorName sigName)
  ^/^ (!^ "include M")
  ^/^ (valueADTDecl m)
  ^/^ (interpDefn m)
  ^/^ (!^ "end")  
