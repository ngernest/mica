open Base
open PPrint
open ParserTypes
open Parser
open ModuleParser
open Utils

(** This file contains the logic for generating PBT code from the AST 
    of a parsed module signature. This file also calls some helper 
    functions defined in the module [Lib.Utils]. *)

(** {1 Functions for generating PBT code} *)

(** [imports filepath] prints out a PPrint document that imports
    the requisite modules for the PBT code.
    The [sigName, modName1, modName2] arguments are the names of the 
    module signatures & the two module implementations, which must
    be the same as their corresponding [.ml] files. *)
let imports (sigName : string) ~(modName1 : string) ~(modName2 : string) : document = 
  comment (!^ "Generated property-based testing code")
  ^/^ (!^ "open Base") 
  ^/^ (!^ "open Base_quickcheck")
  ^/^ !^ ("open " ^ sigName)
  ^/^ !^ ("open " ^ modName1)
  ^/^ !^ ("open " ^ modName2)
  ^^ hardline

(** Document for printing the PPX annotation for S-Expr serialization (indented),
    followed by a newline *)
let sexpAnnotation : document = 
  blank 2 ^^ !^ "[@@deriving sexp]" ^^ hardline   

(** [isArrowType v] returns true if the value declaration [v] has an arrow type *)  
let isArrowType (v : valDecl) : bool = 
  match valType v with 
  | Func1 _ | Func2 _ -> true 
  | _ -> false  

(** [tyIsArrow ty] returns true if [ty] correspons to an arrow type *)    
let tyIsArrow (ty : ty) : bool = 
  match ty with 
  | Func1 _ | Func2 _ -> true 
  | _ -> false    
    
(** Extracts all the "identity elements" defined within a module signature,
    e.g. [mempty] for monoids, or [zero] & [one] for semirings
    - For our purposes, we assume any declaration of the form [val x : t] 
      in the module signature, where [t] is the module's abstract type, 
      is an identity element *)
let getIdentityElements (m : moduleSig) =
  let collectIdElts (acc : string list) (v : valDecl) : string list = 
    begin match valType v with 
    | T | AlphaT -> (String.capitalize @@ valName v) :: acc
    | _ -> acc
    end in 
  List.fold ~init:[] ~f:collectIdElts m.valDecls

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
    ^^ (!^ (string_of_ty ~alpha:"int" arg))
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
  (barSpace ^^ (group @@ 
    separate_map (hardline ^^ barSpace) extractArgTypes m.valDecls 
    ^/^ sexpAnnotation))  

(** Helper function for printing out OCaml constructors
    (Wrapper for the [OCaml.variant] function in the [PPrint] library) *)    
let printConstructor (constr : string) (args : string list) : document = 
  match args with 
  | [] -> !^ constr
  | [arg] -> !^ constr ^^ blank 1 ^^ !^ arg
  | _ -> OCaml.variant "expr" constr 1 (List.map ~f:string args)

(** [sanitizeString s] uncapitalizes the string [s] and filters out chars
    corresponding to parentheses *)  
(* let sanitizeString (s : string) = 
  let open String in 
  filter (uncapitalize s) ~f:(fun c -> let open Char in c <> '(' && c <> ')') *)

(** Converts a string denoting a type to the equivalent [ty] ADT constructor *)  
let ty_of_string (s : string) : ty = 
  let open String in 
  let s' = s |> lowercase
    |> chop_suffix_if_exists ~suffix:"option" 
    |> chop_suffix_if_exists ~suffix:"list" 
    |> filter ~f:(fun c -> let open Char in c <> '(' && c <> ')') 
    |> strip in 
  match (run_parser typeP s') with 
  | Ok ty -> ty 
  | Error err -> 
    failwith @@ Printf.sprintf 
      "Error %s : couldn't parse the string \"%s\"\n" err s
     


  (* let open String in 
  let strs = List.map ~f:sanitizeString (split_on_chars ~on:[' '; '*'; '('; ')'] s) in
  match strs with 
  | [] -> failwith "String is empty"
  | [str] -> 
    begin match str with 
    | "int" -> Int 
    | "char" -> Char 
    | "unit" -> Unit 
    | "bool" -> Bool
    | "string" -> String
    | "\'a" -> Alpha 
    | "\'a t" -> AlphaT 
    | "t" -> T 
    | _ -> 
      (* If [str] contains the substring "Option", convert to an option type *)
      if is_substring ~substring:"Option" str
        then let newTy = substr_replace_first ~pattern:"Option" ~with_:"" str in 
          Option (ty_of_string newTy)
      else if is_substring ~substring:"List" str 
        then let newTy = substr_replace_first ~pattern:"List" ~with_:"" str in 
          List (ty_of_string newTy)
      else failwith (Printf.sprintf "Singleton case: type conversion from \"%s\" not supported" str)
    end
  | [str ; "option"] -> Option (ty_of_string str)
  | [str ; "list"] -> List (ty_of_string str)
  | ss -> failwith @@ Printf.sprintf "Type conversion from \"%s\" not supported" 
                     (Sexp.to_string (sexp_of_list sexp_of_string ss)) *)

(** [varNameHelper ty] returns an appropriate variable name corresponding 
    to [ty], eg. [varNameHelper Int = n] *)  
let rec varNameHelper (ty : ty) : string = 
  match ty with 
  | Alpha -> "x"
  | T | AlphaT -> "e"
  | Int -> "n"
  | Bool -> "b"
  | Char -> "c"
  | List argTy -> varNameHelper argTy ^ "s"
  | Option argTy -> varNameHelper argTy
  (* TODO: check if we need to destruct the pair *)
  | Pair (_, _) -> "p" 
  | _ -> String.prefix (string_of_ty ty) 1

(** Special case of [genVarNames] when we only have one argument type.  
    If [prime = true], add a single quote to the end of the variable name *)    
let genVarNamesSingleton ?(prime = false) (argTy : ty) : string = 
  let varName = varNameHelper argTy in 
  if prime then varName ^ "\'" else varName

(** Generates [n] unique variable names corresponding to a type [ty]. 
    For example, [genVarNamesN 3 Int] produces [["n1"; "n2"; "n3"]].  *)  
let genVarNamesN ~(n : int) (ty : ty) : string list = 
  replicate ~n (varNameHelper ty) |> 
  List.mapi ~f:(fun i arg -> arg ^ Int.to_string @@ i + 1)

(** Takes a list of argument types, and generates corresponding variable names 
    which are unique for each element of the list 
    eg. [genVarNames [Int; Int; Char] = [n1; n2; c]] *)
let genVarNames ?(prime = false) (argTys : ty list) : string list = 
  match argTys with 
  | [] -> []
  | [ty] -> [genVarNamesSingleton ty]
  | _ -> List.mapi 
    ~f:(fun i ty -> let var = genVarNamesSingleton ty ^ Int.to_string (i + 1) in 
        if prime then var ^ "\'" else var) 
    argTys

(** Fetches the [expr] constructor corresponding to a [val] declaration 
    in a module *)
let getExprConstructorName (v : valDecl) : string = 
  String.capitalize @@ valName v
    
(** Fetches the constructor corresponding to a [val] 
    declaration in the [expr] ADT, 
    returning a pair of the form [(args, constructor applied to args)], 
    eg. [(["x", "e"], !^ "Mem(x,e)")] 
    - The auxiliary argument [idElts] is a list of names of the identity elements
    defined in the module signature (eg. [mempty] for monoids) 
    - See [getIdentityElements] for further details on identity elements *)
let getExprConstructor ~(idElts : string list) (v : valDecl) : string list * document = 
  let constr = getExprConstructorName v in
  (* Check if the constructor corresponds to an identity element in the signature *)
  let isIdElt = List.mem idElts constr ~equal:String.equal in 
  match isIdElt, valType v with 
  | true, _ -> ([], !^ constr)
  | _, Int -> (["n"], printConstructor constr ["n"])
  | _, Char -> (["c"], printConstructor constr ["c"])
  | _, Bool -> (["b"], printConstructor constr ["b"])
  | _, Unit -> ([], OCaml.unit)
  | _, String -> (["s"], printConstructor constr ["s"])
  | _, Alpha -> (["a"], printConstructor constr ["a"])
  | _, T | _, AlphaT -> (["t"], printConstructor constr ["t"])
  | _, Option argTy | _, List argTy | _, Func1 (argTy, _) -> 
    let arg = genVarNames [argTy] in 
    (arg, printConstructor constr arg)
  | _, Pair (arg1, arg2) | _, Func2 (arg1, arg2, _) -> 
    let args = genVarNames [arg1; arg2] in 
    (args, printConstructor constr args)

(** Extracts the return type of a function.  
    For non-arrow types, this function just extracts the type itself *)    
let extractReturnType (v : valDecl) : string = 
  match valType v with 
  | Func1 (_, ret) | Func2 (_, _, ret) -> 
    string_of_ty ~t:"T" ~alpha:"Int" ~camelCase:true ret
  | ty -> string_of_ty ~t:"T" ~alpha:"Int" ~camelCase:true ty

(** Fetches the unique return types across the functions / values 
    in a module signature *)  
let uniqRetTypesInSig (m : moduleSig) : string list = 
  let open String in
  List.dedup_and_sort ~compare:compare
    @@ List.map ~f:(fun v -> extractReturnType v |> capitalize) m.valDecls

(** Generates the definition of the [ty] ADT *)  
let tyADTDecl (m : moduleSig) : document = 
  let retTypes = uniqRetTypesInSig m in 
  prefix 2 1
  (!^ "type ty =")
  (group @@ separate_map (!^ " | ") (!^) retTypes
    ^/^ sexpAnnotation
    ^^ hardline)

(** Given a [val] declaration inside a module (eg. [val f : 'a -> 'a]), 
    returns the corresponding function name (eg. [M.f]) *)
let getFuncName (v : valDecl) : document = 
  !^ ("M." ^ valName v)     

(** Like [valADTConstructor] (see below), but returns a string 
    instead of a PPrint document *)  
let valADTConstructorString (ty : string) : string = 
  "Val" ^ String.capitalize ty

(** [valADTConstructor ty] generates the constructor name for the 
    [value] ADT corresponding to the type [ty] *)  
let valADTConstructor (ty : string) : document = 
  !^ (valADTConstructorString ty)

(** Generates the list of constructor names for the [ty] ADT *)  
let tyADTConstructors (m : moduleSig) : string list =
  uniqRetTypesInSig m

(** Returns an association list where each element is a constructor for the 
    [ty] ADT & its associated constructor for the [value] ADT *)  
let tyAndValADTConstructors (m : moduleSig) : (string * string) list = 
  List.map ~f:(fun ty -> ty, valADTConstructorString ty) 
  (tyADTConstructors m)

(** Takes [s], a string reprentation of a type, 
    and instantiates the abstract type [t] to [int M.t] 
    where [M] is some module defined elsewhere. 
    - Example: [instantiate "toption" = "int M.t option"] 
    - If [s] doesn't contain the prefix ["t"], [s] is left unchanged  *)  
let instantiateT (s : string) = 
  let open String in 
  if is_prefix ~prefix:"t" s 
  then "int M.t" ^ drop_prefix s 1
  else s

(** Takes [s], a string reprentation of a type, 
    and adds a space to [s] if [s] represents a parameterized type. 
    - Example: [addSpaceToTyStr "intoption" = "int option"] 
    - If [s] doesn't contain the prefix ["t"], [s] is left unchanged *)
let addSpaceToTyStr (s : string) = 
  let open String in 
  if is_suffix ~suffix:"option" s 
    then chop_suffix_exn ~suffix:"option" s ^ " option"
  else if is_suffix ~suffix:"list" s 
    then chop_suffix_exn ~suffix:"list" s ^ " list"
  else s

(** [valADTParam moduleAbsTy ty] generates the type param for the 
    constructor [value] ADT corresponding to the type [ty]
    - The auxiliary argument [moduleAbsTy] refers to the abstract type 
      contained within the module signature, e.g. [M.t]
    - If the module's abstract type [M.t] is polymorphic (i.e. ['a M.t]), 
      we instantiate ['a] with [int], 
      otherwise we leave the abstract type monomorphic (i.e. just [M.t]) *)    
let valADTParam ~(moduleAbsTy : abstractType) (ty : string) : document = 
  let open String in
  match (lowercase ty |> split ~on:' ') with 
  | [] -> failwith "Can't extract a type parameter from an empty string"
  | [str] -> 
    if str = "t" then
      begin match moduleAbsTy with 
      | T0 -> !^ "M.t"
      | T1 _ -> !^ "int M.t" 
      end
    else !^ (addSpaceToTyStr str |> instantiateT)
  | tys -> separate_map space (!^) tys  

(** [valADTTypeDef moduleAbsTy ty] generates both the constructor & type parameter 
    for the [value] ADT corresponding to the type [ty]
    - The auxiliary argument [moduleAbsTy] is the abstract type defined in the module, 
      which is used to determine if the abstract type needs to be instantiated
      with a concrete type (this logic is handled in [valADTParam])
    - Note: This function is a helper function called by [valADTDefn] *)  
let valADTTypeDef ~(moduleAbsTy : abstractType) (ty : string) : document = 
  valADTConstructor ty 
  ^^ (!^ " of ")
  ^^ valADTParam ~moduleAbsTy ty      

(** Generates the [value] ADT definition that is contained within 
    the module returned by the [ExprToImpl] functor *)  
let valADTDefn (m : moduleSig) : document = 
  let valueTypes : string list = uniqRetTypesInSig m in 
  prefix 2 1 
  (!^ "type value = ")
  (barSpace ^^ 
    (group @@ separate_map (hardline ^^ barSpace) 
      (fun ty -> valADTTypeDef ~moduleAbsTy:m.abstractType ty) 
      valueTypes 
    ^/^ sexpAnnotation))  

(** Given an argument and its type, determines if we need to recursively call 
    [interp] on the argument for the inner pattern match in [interp] *)
let interpIsNeeded (argTy : ty) : bool = 
  match argTy with 
  | AlphaT | T -> true
  | _ -> false

(** Auxiliary data type for indicating the position of a non-[expr] argument
    to a function *)
type argPos = Fst | Snd
  [@@deriving sexp]

(** Pattern matches [interp] on one argument of type [expr].  
    If [nonExprArg] is [Some] of some value, it is placed
    in the appropriate argument position during function application. *)  
let interpOnce (argTy : ty) ?(nonExprArg = None) (funcName : document) (arg : ident) (retTy : ty) : document = 
  let open PPrint in 
  (* Obtain appropriate constructors based on the arg & return types *)    
  let (argTyConstr, retTyConstr) = 
    map2 ~f:(Fn.compose valADTConstructor (string_of_ty ~t:"T" ~alpha:"Int" ~camelCase:true)) (argTy, retTy) in
  (* Generate a fresh variable name *)  
  let arg' = genVarNamesSingleton ~prime:true argTy in
  (* Identify the position of any arguments whose type are not [expr] *)
  let funcApp = 
    begin match nonExprArg with 
    | None -> funcName ^^ space ^^ (!^ arg') 
    | Some (nonExprArg, Fst) -> funcName ^^ (spaceLR (!^ nonExprArg) ^^ (!^ arg'))
    | Some (nonExprArg, Snd) -> funcName ^^ (spaceLR (!^ arg') ^^ (!^ nonExprArg))
    end in 
  align @@ (!^ "begin match interp ") ^^ (!^ arg) ^^ (!^ " with ")
    ^/^ (!^ " | ") ^^ argTyConstr
    ^^ (space ^^ !^ arg') 
    ^^ sArrow ^^ spaceR retTyConstr ^^ parens funcApp
    ^/^ (!^ " | _ -> failwith " ^^ OCaml.string "impossible")
    ^/^ (!^ "end")

(** Pattern matches [interp] on two arguments, both of type [expr] *)      
let interpTwice (arg1Ty : ty) (arg2Ty : ty) (funcName : document) 
                (arg1 : ident) (arg2 : ident) (retTy : ty) : document = 
  let open PPrint in 
  (* Obtain appropriate constructors based on the arg & return types *)              
  let (arg1TyConstr, arg2TyConstr, retTyConstr) = 
    map3 ~f:(Fn.compose valADTConstructor (string_of_ty ~t:"T" ~alpha:"Int" ~camelCase:true)) (arg1Ty, arg2Ty, retTy) in
  (* Generate fresh variable names *)    
  match List.map ~f:(!^) (genVarNames ~prime:true [arg1Ty; arg2Ty]) with 
   | [arg1'; arg2'] -> 
     align @@ (!^ "begin match ") 
      ^^ (OCaml.tuple [!^ ("interp " ^ arg1); !^ ("interp " ^ arg2)]) 
      ^^ (!^ " with ")
      ^/^ (!^ " | ") ^^ OCaml.tuple [arg1TyConstr ^^ space ^^ arg1'; arg2TyConstr ^^ space ^^ arg2']
      ^^ sArrow ^^ spaceR retTyConstr ^^ parens (funcName ^^ (spaceLR arg1') ^^ arg2')
      ^/^ (!^ " | _ -> failwith " ^^ OCaml.string "impossible")
      ^/^ (!^ "end")
  | _ -> failwith "error generating fresh variable names"

(** Produces the inner pattern match ([interp e]) in the [interp] function *) 
let interpExprPatternMatch (v, args : valDecl * string list) : document = 
  let open PPrint in 
  let funcName = getFuncName v in
  match valType v, args with 
  | Func1 (argTy, retTy), [arg] -> 
    if interpIsNeeded argTy
    then interpOnce argTy funcName arg retTy
    else 
      (* Generate the appropriate constructor based on the return type *)
      let retTyConstr = valADTConstructor (string_of_ty ~t:"T" ~alpha:"Int" retTy) in
      retTyConstr ^^ space ^^ parens (funcName ^^ space ^^ (!^ arg))
  | Func2 (arg1Ty, arg2Ty, retTy), [arg1; arg2] -> 
    begin match interpIsNeeded arg1Ty, interpIsNeeded arg2Ty with 
    | true, true -> interpTwice arg1Ty arg2Ty funcName arg1 arg2 retTy
    | true, _ -> interpOnce arg1Ty ~nonExprArg:(Some (arg2, Snd)) funcName arg1 retTy 
    | _, true -> interpOnce ~nonExprArg:(Some (arg1, Fst)) arg2Ty funcName arg2 retTy 
    | _, _ -> 
      let retTyConstr = valADTConstructor (string_of_ty ~t:"T" ~alpha:"Int" retTy) in
      retTyConstr ^^ space ^^ parens (funcName ^^ spaceLR (!^ arg1) ^^ (!^ arg2))
    end
  | valTy, _ -> 
    let valTyConstr = valADTConstructor (string_of_ty ~t:"T" ~alpha:"Int" valTy) in
    valTyConstr ^^ space ^^ parens funcName

(** [interpDefn m] generates the definition of the [interp] function, 
    which evaluates [expr]s based on the value declarations defined in the 
    module signature [m]. This function does so by doing the following:
    - Extracting the names of all the identity elements in [m]
      by calling [getIdentityElements]
    - Fetching the [expr] ADT constructors & arguments based on the 
      declarations inside [m]
    - Generating the pattern matches inside the [interp] function *)
let interpDefn (m : moduleSig) : document = 
  let idElts = getIdentityElements m in 
  let (exprConstrArgs, exprConstrs) = 
    List.unzip @@ List.map ~f:(getExprConstructor ~idElts) m.valDecls in
  let innerPatternMatches = 
    List.map ~f:interpExprPatternMatch (List.zip_exn m.valDecls exprConstrArgs) in
  (** [interpHelper i constr] takes in a PPrint document [constr] 
    at index [i], and concatenates it with the appropriate pattern matching code *)    
  let interpHelper (i : int) (exprConstr : document) : document = 
    let pattern = List.nth_exn innerPatternMatches i in 
    break 1 
    ^^ sBar ^^ exprConstr ^^ sArrow 
    ^^ jump 2 0 pattern
  in
  hang 2 @@ 
  !^ "let rec interp (expr : expr) : value = " 
  ^/^ (!^ "match expr with")
  ^^ (concat (List.mapi ~f:interpHelper exprConstrs)) 
  ^^ break 1

(** Generates the definition of the [ExprToImpl] functor *)  
let functorDef (m : moduleSig) ~(sigName : string) ~(functorName : string) : document = 
  hang 2 @@ 
  !^  (Printf.sprintf "module %s (M : %s) = struct " functorName sigName)
  ^/^ (!^ "include M" ^^ hardline)
  ^/^ (valADTDefn m)
  ^/^ (interpDefn m)
  ^/^ (!^ "end")  
  ^^ hardline

(** [getGenerator ty] takes in a type [ty] and produces a PPrint document
    containing the corresponding QuickCheck generator for that type. 
    - The auxiliary argument [nonNegOnly] specifies whether QuickCheck's int 
      generators should only generate non-negative ints (in the event [ty] is [Int]). 
    - By default, the int generator returned generates both 
      negative & positive ints. 
    - Note that polymorphic types (eg. [Alpha]) are instantiated as ints
    - This is a helper function called by [argGen]. *)
let rec getGenerator ?(nonNegOnly = false) (ty : ty) : document = 
  let open PPrint in 
  match ty with 
  | Int | Alpha     -> 
    if nonNegOnly then !^ "G.small_positive_or_zero_int"
    else !^ "G.int_inclusive (-10) 10"
  | Char            -> !^ "G.char_alpha"
  | Bool            -> !^ "G.bool"
  | Unit            -> !^ "G.unit"
  | String          -> !^ "G.string_non_empty"
  | Option argTy    -> !^ "G.option @@ " ^^ getGenerator ~nonNegOnly argTy
  | List argTy      -> !^ "G.list @@ " ^^ jump 2 1 @@ getGenerator ~nonNegOnly argTy
  | Pair (ty1, ty2) -> 
    let (g1, g2) = map2 ~f:(getGenerator ~nonNegOnly) (ty1, ty2) in 
    !^ "G.both" ^^ spaceLR (parens g1) ^^ parens g2
  | _ -> failwith @@            
    Printf.sprintf "Error: Can't fetch generator for type %s" 
    (string_of_ty ~t:"T" ~alpha:"Alpha" ty)

(** Produces the code for a monadic bind of [arg] to a QuickCheck generator 
    producing a value of type [ty], where [ty] must be a non-arrow type.
    - The auxiliary argument [nonNegOnly] specifies whether QuickCheck's int 
      generators should only generate non-negative ints
    - This is a helper function called by [genExprPatternRHS]. *)  
let argGen ?(nonNegOnly = false) (arg : string) (ty : ty) : document = 
  let open Printf in 
  let binding = !^ (sprintf "let%%bind %s = " arg) in
  match ty with 
  | Int | Alpha       -> binding ^^ getGenerator ~nonNegOnly Int  ^^ sIn
  | Char              -> binding ^^ getGenerator ~nonNegOnly Char ^^ sIn
  | Bool              -> binding ^^ getGenerator ~nonNegOnly Bool ^^ sIn
  | Unit              -> binding ^^ getGenerator ~nonNegOnly Unit ^^ sIn
  | String            -> binding ^^ getGenerator ~nonNegOnly String ^^ sIn
  | Option _ | List _ -> binding ^^ getGenerator ~nonNegOnly ty   ^^ sIn
  | Pair (ty1, ty2)   -> 
    let lst = if phys_equal ty1 ty2 
      then genVarNamesN ~n:2 ty1 
      else genVarNames [ty1; ty2] in 
    begin match lst with 
    | [v1; v2] -> 
      !^ (sprintf "let%%bind (%s, %s) = " v1 v2) 
        ^^ getGenerator ~nonNegOnly ty ^^ sIn 
    | _ -> failwith "Error generating fresh varnames for Pair type"
    end 
  | T | AlphaT -> 
    !^ (sprintf "let%%bind %s = G.with_size ~size:(k / 2) (gen_expr %s) in " 
        arg (string_of_ty ~t:"T" ~alpha:"Alpha" ty))
  | _ -> failwith "Higher-order functions not supported"

(** Takes in the following arguments:
    - [nonNegOnly]: An optional argument specifying whether QuickCheck's int 
        generators should only generate non-negative ints
    - [tyConstr]: constructor for the [ty] ADT representing the return type of a function
    - [funcTy]: function type
    - [args]: arguments passed to the [Expr] constructor
    - [constr]: constructor for the [Expr] ADT
    - [funcApp]: PPrint document containing the application of [constr] onto [args]

    This function produces the RHS of the pattern matches in [gen_expr],
    returning a tuple of the form 
    [(tyConstr, (ty, patternMatchRHS, nameOfPatternMatch))]
    
    For example, if:
    - [tyConstr = Bool]
    - [ty = Func2(Alpha, Expr, Bool)]
    - [constr = Mem]
    - [funcApp = Mem(x, e)]
    then [genExprPatternRHS (tyConstr, ty, args, constr, funcApp)] produces the code
    {[
      let%bind arg = G.int in G.return @@ Mem(x, e)
    ]}
    where [G] = [Base_quickcheck.Generator]. *)  
let genExprPatternRHS 
  ?(nonNegOnly = false)
  (tyConstr, funcTy, args, constr, funcApp : string * ty * string list * string * document) 
  : string * (ty * document * string) = 
  let patternName = String.uncapitalize constr in 
  let preamble = !^ ("let " ^ patternName ^ " = ") in
  let monadicReturn = !^ "G.return @@ " ^^ funcApp in
  match funcTy, args with 
  | Func1 (argTy, _), [arg] -> 
    let patternRHS = 
      preamble ^^ jump 2 1 @@ argGen ~nonNegOnly arg argTy ^/^ monadicReturn in
    tyConstr, (funcTy, patternRHS, patternName)
  | Func2 (arg1Ty, arg2Ty, _), [arg1; arg2] -> 
    let patternRHS = 
      preamble ^^ jump 2 1 @@ argGen ~nonNegOnly arg1 arg1Ty 
        ^/^ argGen ~nonNegOnly arg2 arg2Ty ^/^ monadicReturn in
    tyConstr, (funcTy, patternRHS, patternName)
  | _ -> tyConstr, (funcTy, (!^ "failwith ") ^^ OCaml.string "TODO", "no pattern")

(** Takes in [tyConstr] (constructor for the [ty] ADT representing the return type of a function), 
    [ty] (the type of the [Expr] construcctor), and 
    [constr] (the string representation of an [Expr] constructor),
    and returns a 5-tuple of the form 
    [(tyConstr, ty, constructorArgs, constructor, constructor applied to args)], 
    eg. [(Bool, Func2(Alpha, Expr, Bool), ["x", "e"], "Mem", !^ "Mem(x,e)")] *)
let getExprConstructorWithArgs (tyConstr : string) 
  (ty : ty) (constr : string) : string * ty * string list * string * document = 
  match ty, constr with 
  | _, "Empty" -> (tyConstr, ty, [], constr, !^ constr)
  | Int, _ -> (tyConstr, ty, ["n"], constr, printConstructor constr ["n"])
  | Char, _ -> (tyConstr, ty, ["c"], constr, printConstructor constr ["c"])
  | Bool, _ -> (tyConstr, ty, ["b"], constr, printConstructor constr ["b"])
  | Unit, _ -> (tyConstr, ty, [], constr, OCaml.unit)
  | String, _ -> (tyConstr, ty, ["s"], constr, printConstructor constr ["s"])
  | Alpha, _ -> (tyConstr, ty, ["a"], constr, printConstructor constr ["a"])
  | T, _ | AlphaT, _ -> 
    (tyConstr, ty, ["t"], constr, printConstructor constr ["t"])
  | Option argTy, _ | List argTy, _ | Func1 (argTy, _), _ ->
    let arg = genVarNames [argTy] in 
    (tyConstr, ty, arg, constr, printConstructor constr arg)
  | Pair (arg1, arg2), _ | Func2 (arg1, arg2, _), _ -> 
    let args = genVarNames [arg1; arg2] in 
    (tyConstr, ty, args, constr, printConstructor constr args)
    
(** Returns an association list of constructors for the [ty] ADT 
  where each element is the form [(<constructor for the ty ADT>, ty)] *)    
let genExprPatterns (m : moduleSig) = 
  let open List in
  m.valDecls 
    |> filter ~f:(fun v -> tyIsArrow (valType v)) 
    |> map ~f:(fun v -> 
        (String.capitalize @@ extractReturnType v, 
         valType v, 
         getExprConstructorName v))
    |> map ~f:(uncurry3 getExprConstructorWithArgs)
    |> map ~f:(genExprPatternRHS 
        ~nonNegOnly:(phys_equal m.intFlag NonNegativeOnly))

(** [setNonNegIntFlag m] sets the [intFlag] field of the module signature [m]
    to [NonNegativeOnly], indicating that QuickCheck's int generators
    should only generate non-negative ints. *)    
let setNonNegIntFlag (m : moduleSig) : moduleSig = 
  { m with intFlag = NonNegativeOnly }

(** Produces the definition of the [gen_expr] Quickcheck generator for 
    the [expr] datatype. 
    - The auxiliary argument [nonNegOnly] is a boolean specifying if QuickCheck's 
      integer generators should only generate non-negative integers. 
      (This is an optional command-line flag that is passed in when Mica is invoked.)
    - An example of the automatically-produced code 
      for [gen_expr] can be found in [GeneratedSetPBTCode.ml] (specialized
      to the finite set example discussed in the README / documentation homepage). 
    - When the internal size parameter of [gen_expr] reaches 0, 
      the function yields a trivial generator that just returns 
      the identity element (or one of them, if there are multiple) 
      of the signature. 
    - See [getIdentityElements] for a further discussion
      on identity elements. *)  
let genExprDef ~(nonNegOnly : bool) (modSig : moduleSig) : document = 

  let m = if nonNegOnly then setNonNegIntFlag modSig else modSig in 

  (* Generator for the identity element(s) in the module signature *)
  let idEltGenerator : document = 
    let idElts = getIdentityElements m in 
    begin match idElts with 
    | [] -> failwith "Missing identity element(s) in module signature"
    | [mempty] -> (!^ "return ") ^^ (!^ mempty)
    | _ -> !^ "G.union " ^^ OCaml.list (fun e -> !^ "G.return " ^^ !^ e) idElts 
    end in 

  (* [patterns] is a [(tyConstr, (funcTy, patternRHS, patternName) list)] *)
  let patterns : (string, (ty * document * string) list) List.Assoc.t = 
    genExprPatterns m
      |> List.Assoc.sort_and_group ~compare:compare_string in

  let completePatterns = 
    List.map patterns ~f:(fun (tyConstr, patternRHSes) -> 
      let patternLHS = OCaml.tuple [!^ tyConstr; underscore] in

      let patternRHS = 
        begin match patternRHSes with 
        | [] -> (!^ "failwith ") ^^ OCaml.string "Error generating [gen_expr]" 
        | [(_, rhs, patternName)] -> rhs ^/^ !^ ("in " ^ patternName)
        | patterns -> 
          let (_, rhses, patternNames) = List.unzip3 patterns in 
          separate (!^ " in " ^^ hardline) rhses ^/^ (!^ "in G.union ") ^^ OCaml.list (!^) patternNames
        end in 

      hardline ^^ sBar ^^ 
      patternLHS ^^ sArrow ^^ jump 4 0 patternRHS) in 
  
  hardline
  ^^ hang 2 @@ 
  !^ "let rec gen_expr (ty : ty) : expr Generator.t = " 
  ^/^ (!^ "let module G = Generator in ")
  ^/^ (!^ "let open G.Let_syntax in ")
  ^/^ (!^ "let%bind k = G.size in ")
  ^/^ (!^ "match ty, k with ")
  ^/^ (sBar ^^ OCaml.tuple [!^ "T"; OCaml.int 0] ^^ sArrow ^^ idEltGenerator)
  ^^ concat completePatterns

(** Produces a PPrint document of an OCaml functor application, where 
    [functorName] is the name of the functor, and [arg] is the name
    of the argument to the functor *)  
let functorApp ~(functorName : moduleName) (arg : string) : document = 
  OCaml.variant "functorApp" functorName 1 [!^ arg]

(** Produces the module bindings for the two module implemntations [I1, I2], 
    where [functorName] is the name of the functor that procduces the PBT test harness,
    and [modName1] & [modName2] are the names of the two module implementaitons *)  
let implModuleBindings ~(functorName : moduleName) (modName1 : string) (modName2 : string) : document = 
  let i1 = !^ "module I1 = " ^^ (functorApp ~functorName modName1) in 
  let i2 = !^ "module I2 = " ^^ (functorApp ~functorName modName2) in
  hardline ^/^ i1 ^/^ i2 ^/^ hardline

(***********************************************************************************)
(** {1 Functions for generating the executable for comparing two modules} *)

(** Generates the definition of a [displayError] helper function 
    for displaying error messages when QuickCheck tests fail *)  
let displayErrorDef : document = 
  !^ "let displayError (e : expr) (v1 : I1.value) (v2 : I2.value) : string = "
  ^^ jump 2 1 @@ !^ "Printf.sprintf " ^^ OCaml.string "e = %s, v1 = %s, v2 = %s\n" 
  ^^ jump 2 1 @@ !^ "(Sexp.to_string @@ sexp_of_expr e)"
  ^/^ !^ "(Sexp.to_string @@ [%sexp_of: I1.value] v1)"
  ^/^ !^ "(Sexp.to_string @@ [%sexp_of: I2.value] v2)"

(** Generate requisite imports for the executable file, where 
    [filepath] is the filepath to the executable file *)  
let executableImports ~(pbtFilePath : string) ~(execFilePath : string) : document = 
  let open Core.Filename in 
  comment (!^ "Generated executable for testing observational equivalence of two modules")
  ^/^ comment (!^ "Usage: " ^^ 
    brackets @@ !^ (Printf.sprintf "dune exec -- %s.exe" @@ chop_extension execFilePath))
  ^/^ !^ ("open Core")
  ^/^ !^ ("open " ^ "Lib." ^ String.capitalize @@ getModuleSigName pbtFilePath)
  ^/^ hardline

(** Produces Quickcheck code in the executable that tests two modules 
    for observational equivalence based on [expr]s that return some [tyConstr] of type [ty], 
    and pattern matching on the equivalent [valConstr]s that they return *)
let obsEquiv (tyConstr, valConstr : string * string) : document = 
  let baseTy : string = addSpaceToTyStr (String.lowercase tyConstr) in
  let vars : string list = genVarNamesN ~n:2 (ty_of_string baseTy) in
  let varDocs : document list = List.map ~f:(fun var -> !^ valConstr ^^ space ^^ !^ var) vars in 

  !^ "QC.test (gen_expr " ^^ (!^ tyConstr) ^^ !^ ") ~sexp_of:sexp_of_expr ~f:(fun e ->"
  ^^ jump 2 1 @@ 
  !^ "match (I1.interp e, I2.interp e) with "
  ^/^ sBar ^^ OCaml.tuple varDocs ^^ sArrow ^^ (!^ "[%test_eq: " ^^ (!^ baseTy) ^^ !^ "] ")
  ^^ separate_map space (!^) vars 
  ^/^ sBar ^^ (!^ "v1, v2") ^^ sArrow ^^ (!^ "failwith @@ displayError e v1 v2);") 
  ^^ hardline

(** [isExcludedType ty] returns true if the type [ty] is 
    a return type to be excluded from consideration when 
    testing for {i observational equivalence}. 
    - For example, expressions returning abstract types [T] and [AlphaT] 
    are excluded when we check if two modules for observational equivalence,
    because these abstract types may be instantiated differently in the two modules. 
    - For instance, one module could instantiate ['a t] to be ['a list], 
    while the other instantiates ['a t] to be ['a tree].
    - Note that arrow types are not considered by this function. *)  
let rec isExcludedType (ty : ty) : bool = 
  match ty with 
  | T | AlphaT -> true 
  | Option argTy | List argTy -> isExcludedType argTy
  | Pair (ty1, ty2) -> isExcludedType ty1 || isExcludedType ty2
  | _ -> false 

(** Generate the executable code for testing observational equivalence
    of two modules *)  
let compareImpls (m : moduleSig) : document = 
  let constrs = List.filter (tyAndValADTConstructors m) 
    ~f:(fun (ty, _) -> not @@ isExcludedType (ty_of_string ty)) in 
  !^ "let () = "
  ^^ jump 2 1 @@ !^ "let module QC = Quickcheck in "
  ^/^ separate_map hardline obsEquiv constrs