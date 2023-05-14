open! Base
open! PPrint
open! ParserTypes

(** [spaced doc] adds a space on either side of the PPrint document [doc] *)  
let spaced (doc : document) : document = 
  enclose space space doc   

(** Aliases for PPrint documents for common OCaml symbols *)  
let sBar : document = spaced bar  
let sArrow : document = spaced (!^ "->")

(** Given a filepath to a .ml/.mli file, retrieves the corresponding name of the 
    top-level module signature (must be the same as the .ml/.mli file) *)  
let getModuleSigName (filepath : string) : string =
  Core.Filename.(basename filepath |> chop_extension)
    
(** [imports filepath] prints out a PPrint document that imports
    the requisite modules for the PBT code.
    The [filepath] argument should be a POSIX filepath specifying
    the path to the .ml/.mli file containing the module signature under test *)
let imports (filepath : string) : document = 
  let sigFile = getModuleSigName filepath in
  (!^ "open! Base") 
  ^^ hardline 
  ^^ (!^ "open! Base_quickcheck")
  ^^ hardline
  ^^ !^ ("open " ^ sigFile) 
  ^^ hardline

(** Document for printing the PPX annotation for S-Expr serialization (indented),
    followed by a newline *)
let sexpAnnotation : document = 
  blank 2 ^^ !^ "[@@deriving sexp]" ^^ hardline   

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

(** Special case of [genVarNames] when we only have one argument type *)    
let genVarNamesSingleton (argTy : ty) : string = 
  varNameHelper argTy   

(** Takes a list of argument types, and generates corresponding variable names 
    which are unique for each element of the list 
    
    eg. [genVarNames [Int, Int] = [n1, n2]] *)
let genVarNames (argTys : ty list) : string list = 
  match argTys with 
  | [] -> []
  | [ty] -> [genVarNamesSingleton ty]
  | _ -> List.mapi ~f:(fun i ty -> varNameHelper ty ^ Int.to_string (i + 1)) 
    argTys

(** Fetches the [expr] constructor corresponding to a [val] declaration 
    in a module *)
let getExprConstructorName (v : valDecl) : string = 
  String.capitalize @@ valName v
    
(** Fetches the constructor corresponding to a [val] 
    declaration in the [expr] ADT, 
    returning a pair of the form [(args, constructor applied to args)], 
    eg. [(["x", "e"], !^ "Mem(x,e)")]  *)
let getExprConstructor (v : valDecl) : string list * document = 
  let constr = getExprConstructorName v in
  match constr, valType v with 
  | "Empty", _ -> ([], !^ constr)
  | _, Int -> (["n"], printConstructor constr ["n"])
  | _, Char -> (["c"], printConstructor constr ["c"])
  | _, Bool -> (["b"], printConstructor constr ["b"])
  | _, Unit -> ([], OCaml.unit)
  | _, Alpha -> (["a"], printConstructor constr ["a"])
  | _, T | _, AlphaT -> (["t"], printConstructor constr ["t"])
  | _, Func1 (argTy, _) -> 
    let singletonArg = genVarNames [argTy] in 
    (singletonArg, printConstructor constr singletonArg)
  | _, Func2 (arg1, arg2, _) -> 
    let args = genVarNames [arg1; arg2] in 
    (args, printConstructor constr args)


(** Extracts the return type of a function 
    For non-arrow types, this function just extracts the type itself *)    
let extractReturnTypes (v : valDecl) : string = 
  match valType v with 
  | Func1 (_, ret) | Func2 (_, _, ret) -> (string_of_ty ~t:"T" ~alpha:"Int" ret)
  | ty -> (string_of_ty ~t:"T" ~alpha:"Int" ty)

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

(** Given a [val] declaration inside a module (eg. [val f : 'a -> 'a]), 
    returns the corresponding function name (eg. [M.f]) *)
let getFuncName (v : valDecl) : document = 
  !^ ("M." ^ valName v)     

(** [valADTConstructor ty] generates the constructor name for the 
    [value] ADT corresponding to the type [ty] *)  
let valADTConstructor (ty : string) : document = 
  !^ ("Val" ^ String.capitalize ty) 

(** [valADTParam ty] generates the type param for the 
    constructor [value] ADT corresponding to the type [ty] *)    
let valADTParam (ty : string) : document = 
  let open String in
  !^ (uncapitalize ty |> fun ty -> if ty = "t" then "int M.t" else ty)

(** [valADTTypeDef ty] generates both the constructor & type parameter 
    for the [value] ADT corresponding to the type [ty] *)  
let valADTTypeDef (ty : string) : document = 
  valADTConstructor ty 
  ^^ (!^ " of ")
  ^^ valADTParam ty

(** Generates the [value] ADT definition (enclosed within the [ExprToImpl] functor) *)  
let valueADTDefn (m : moduleSig) : document = 
  let valueTypes = uniqRetTypesInSig m in 
  prefix 2 1 
  (!^ "type value = ")
  (group @@ separate_map (!^ " | ") valADTTypeDef valueTypes 
    ^/^ sexpAnnotation)  

(** Given an argument and its type, determines if we need to recursively call 
    [interp] on the argument for the inner pattern match in [interp] *)
let interpIsNeeded (argTy : ty) : bool = 
  match argTy with 
  | AlphaT | T -> true
  | _ -> false

(** Pattern matches [interp] on one argument of type [expr] *)  
let interpOnce (v : valDecl) (arg : ident) (retTy : ty) : document = 
  let retTyValConstr = valADTConstructor (string_of_ty ~t:"T" ~alpha:"Int" retTy) in
  align @@ (!^ "begin match interp ") ^^ (!^ arg) ^^ (!^ " with ")
    ^/^ (!^ " | ") ^^ retTyValConstr
    ^^ (space ^^ !^ (genVarNamesSingleton retTy)) 
    ^^ sArrow ^^ spaced retTyValConstr ^^ parens (getFuncName v ^^ space ^^ (!^ arg))
    ^/^ (!^ " | _ -> failwith " ^^ OCaml.string "impossible")
    ^/^ (!^ "end")

(** Pattern matches [interp] on two arguments both of type [expr] *)      
let interpTwice (v : valDecl) (arg1 : ident) (arg2 : ident) (retTy : ty) : document = 
  let retTyValConstr = valADTConstructor (string_of_ty ~t:"T" ~alpha:"Int" retTy) in
  align @@ (!^ "begin match ") 
    ^^ (OCaml.tuple [!^ ("interp " ^ arg1); !^ ("interp " ^ arg2)]) 
    ^^ (!^ " with ")
    ^/^ (!^ " | ") ^^ retTyValConstr
    ^^ (space ^^ !^ (genVarNamesSingleton retTy)) 
    ^^ sArrow ^^ spaced retTyValConstr ^^ parens (getFuncName v ^^ (spaced !^ arg1) ^^ (!^ arg2))
    ^/^ (!^ " | _ -> failwith " ^^ OCaml.string "impossible")
    ^/^ (!^ "end")


(** Produces the inner pattern match ([interp e]) in the [interp] function *) 
let interpExprPatternMatch (v, args : valDecl * string list) : document = 
  match valType v, args with 
  | Func1 (argTy, retTy), [arg] -> 
    if interpIsNeeded argTy
    then interpOnce v arg retTy
    else getFuncName v ^^ spaced (!^ arg)
  | Func2 (arg1Ty, arg2Ty, retTy), [arg1; arg2] -> 
    begin match interpIsNeeded arg1Ty, interpIsNeeded arg2Ty with 
    | true, true -> interpTwice v arg1 arg2 retTy
    | false, true -> interpOnce v arg1 retTy 
    | true, false -> interpOnce v arg2 retTy 
    | _, _ -> getFuncName v ^^ spaced (!^ arg1) ^^ spaced (!^ arg2)
    end
  | valTy, _ -> 
    (* TODO: handle case for [Is_empty] (should just return [b]) *)
    let valTyConstr = valADTConstructor (string_of_ty ~t:"T" ~alpha:"Int" valTy) in
    valTyConstr ^^ space ^^ parens @@ getFuncName v



(** Generates the definition of the [interp] function which evaluates [expr]s *)
let interpDefn (m : moduleSig) : document = 
  let (exprConstrArgs, exprConstrs) = 
    List.unzip @@ List.map ~f:getExprConstructor m.valDecls in
  let innerPatternMatches = 
    List.map ~f:interpExprPatternMatch (List.zip_exn m.valDecls exprConstrArgs) in
  (** [interpHelper i constr] takes in a PPrint document [constr] 
    at index [i], and concatenates it with the appropriate pattern matching code *)    
  let interpHelper (i : int) (exprConstr : document) : document = 
    (* let constrArgs = List.nth_exn exprConstrArgs i in  *)
    let pattern = List.nth_exn innerPatternMatches i in 
    break 1 
    ^^ sBar ^^ exprConstr ^^ sArrow 
    ^^ jump 2 1 pattern
  in
  hang 2 @@ 
  !^ "let rec interp (expr : expr) : value = " 
  ^/^ (!^ "match expr with")
  (* ^/^ (!^ " | ") *)
  ^^ (concat (List.mapi ~f:interpHelper exprConstrs)) (* TODO: check if calling [concat] is valid *)
  ^^ break 1


(** Generates the definition of the [ExprToImpl] functor *)  
let functorDef (m : moduleSig) ~(sigName : string) ~(functorName : string) : document = 
  hang 2 @@ 
  !^  (Printf.sprintf "module %s (M : %s) = struct " functorName sigName)
  ^/^ (!^ "include M")
  ^/^ (valueADTDefn m)
  ^/^ (interpDefn m)
  ^/^ (!^ "end")  
  ^^ hardline
