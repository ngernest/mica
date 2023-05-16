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
  | Alpha -> "x"
  | T | AlphaT -> "e"
  | Int -> "n"
  | _ -> String.prefix (string_of_ty ty) 1

(** Special case of [genVarNames] when we only have one argument type 
    If [prime = true], add a single quote to the end of the variable name *)    
let genVarNamesSingleton ?(prime = false) (argTy : ty) : string = 
  let varName = varNameHelper argTy in 
  if prime then varName ^ "\'" else varName

(** Takes a list of argument types, and generates corresponding variable names 
    which are unique for each element of the list 
    eg. [genVarNames [Int, Int] = [n1, n2]] *)
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
    @@ List.map ~f:(fun v -> extractReturnTypes v |> capitalize) m.valDecls
    
(** Returns an association list of constructors for the [ty] ADT 
    where each element is the form [(ty, <constructor for the ADT ty>)] *)    
let tyADTConstructorsAssocList (m : moduleSig) : (ty * string) list = 
  let open String in
  m.valDecls 
    |> List.map ~f:(fun v -> (valType v, capitalize @@ extractReturnTypes v))
    |> List.dedup_and_sort 
       ~compare:(fun (_, tyName1) (_, tyName2) -> compare tyName1 tyName2)
    

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

(** Applies a function pointwise on a pair *)  
let map2 ~f (a1, a2) = (f a1, f a2)  

(** Applies a function pointwise on a triple *)  
let map3 ~f (a1, a2, a3) = (f a1, f a2, f a3)  

(** Auxiliary data type for indicating the position of a non-[expr] argument
    to a function *)
type argPos = Fst | Snd
  [@@deriving sexp]

(** Pattern matches [interp] on one argument of type [expr] 
    If [nonExprArg] is [Some] of some value, it is placed
    in the appropriate argument position during function application *)  
let interpOnce (argTy : ty) ?(nonExprArg = None) (funcName : document) (arg : ident) (retTy : ty) : document = 
  (* Obtain appropriate constructors based on the arg & return types *)    
  let (argTyConstr, retTyConstr) = 
    map2 ~f:(Fn.compose valADTConstructor (string_of_ty ~t:"T" ~alpha:"Int")) (argTy, retTy) in
  (* Generate a fresh variable name *)  
  let arg' = genVarNamesSingleton ~prime:true argTy in
  (* Identify the position of any arguments whose type are not [expr] *)
  let funcApp = 
    begin match nonExprArg with 
    | None -> funcName ^^ space ^^ (!^ arg') 
    | Some (nonExprArg, Fst) -> funcName ^^ (spaced (!^ nonExprArg) ^^ (!^ arg'))
    | Some (nonExprArg, Snd) -> funcName ^^ (spaced (!^ arg') ^^ (!^ nonExprArg))
    end in 
  align @@ (!^ "begin match interp ") ^^ (!^ arg) ^^ (!^ " with ")
    ^/^ (!^ " | ") ^^ argTyConstr
    ^^ (space ^^ !^ arg') 
    ^^ sArrow ^^ spaced retTyConstr ^^ parens funcApp
    ^/^ (!^ " | _ -> failwith " ^^ OCaml.string "impossible")
    ^/^ (!^ "end")

(** Pattern matches [interp] on two arguments, both of type [expr] *)      
let interpTwice (arg1Ty : ty) (arg2Ty : ty) (funcName : document) 
                (arg1 : ident) (arg2 : ident) (retTy : ty) : document = 
  (* Obtain appropriate constructors based on the arg & return types *)              
  let (arg1TyConstr, arg2TyConstr, retTyConstr) = 
    map3 ~f:(Fn.compose valADTConstructor (string_of_ty ~t:"T" ~alpha:"Int")) (arg1Ty, arg2Ty, retTy) in
  (* Generate fresh variable names *)    
  match List.map ~f:(!^) (genVarNames ~prime:true [arg1Ty; arg2Ty]) with 
   | [arg1'; arg2'] -> 
     align @@ (!^ "begin match ") 
      ^^ (OCaml.tuple [!^ ("interp " ^ arg1); !^ ("interp " ^ arg2)]) 
      ^^ (!^ " with ")
      ^/^ (!^ " | ") ^^ OCaml.tuple [arg1TyConstr ^^ space ^^ arg1'; arg2TyConstr ^^ space ^^ arg2']
      ^^ sArrow ^^ spaced retTyConstr ^^ parens (funcName ^^ (spaced arg1') ^^ arg2')
      ^/^ (!^ " | _ -> failwith " ^^ OCaml.string "impossible")
      ^/^ (!^ "end")
  | _ -> failwith "error generating fresh variable names"


(** Produces the inner pattern match ([interp e]) in the [interp] function *) 
let interpExprPatternMatch (v, args : valDecl * string list) : document = 
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
      retTyConstr ^^ space ^^ parens (funcName ^^ spaced (!^ arg1) ^^ spaced (!^ arg2))
    end
  | valTy, _ -> 
    let valTyConstr = valADTConstructor (string_of_ty ~t:"T" ~alpha:"Int" valTy) in
    valTyConstr ^^ space ^^ parens funcName


(** Generates the definition of the [interp] function which evaluates [expr]s *)
let interpDefn (m : moduleSig) : document = 
  let (exprConstrArgs, exprConstrs) = 
    List.unzip @@ List.map ~f:getExprConstructor m.valDecls in
  let innerPatternMatches = 
    List.map ~f:interpExprPatternMatch (List.zip_exn m.valDecls exprConstrArgs) in
  (** [interpHelper i constr] takes in a PPrint document [constr] 
    at index [i], and concatenates it with the appropriate pattern matching code *)    
  let interpHelper (i : int) (exprConstr : document) : document = 
    let pattern = List.nth_exn innerPatternMatches i in 
    break 1 
    ^^ sBar ^^ exprConstr ^^ sArrow 
    ^^ jump 2 1 pattern
  in
  hang 2 @@ 
  !^ "let rec interp (expr : expr) : value = " 
  ^/^ (!^ "match expr with")
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

(** Produces the RHS of the pattern matches in [gen_expr] *)  
let genExprPatternRHS (ty, args, funcApp : ty * string list * document) : ty * document = 
  match ty, args with 
  | Func1 (argTy, _), [arg] -> 
    let binding = 
      Printf.sprintf "let%%map %s = G.with_size ~size:(k / 2) (gen_expr %s) in " 
        arg (string_of_ty ~t:"T" ~alpha:"Alpha" argTy) in
    (ty, (!^ binding) ^/^ funcApp)
  (* | Func2 (arg1Ty, arg2Ty, retTy), [arg1; arg2] -> !^ "failwith " ^^ OCaml.string "TODO" *)
  | _ -> (ty, (!^ "failwith ") ^^ OCaml.string "TODO")

(** Takes in a pair of the form (ty, tyADTConstructorString) 
    and returns a triple of the form [(ty, constructorArgs, constructor applied to args)], 
    eg. [(["x", "e"], !^ "Mem(x,e)")] *)
let getExprConstructor' (ty, constr : ty * string) : ty * string list * document = 
  match ty, constr with 
  | _, "Empty" -> (ty, [], !^ constr)
  | Int, _ -> (ty, ["n"], printConstructor constr ["n"])
  | Char, _ -> (ty, ["c"], printConstructor constr ["c"])
  | Bool, _ -> (ty, ["b"], printConstructor constr ["b"])
  | Unit, _ -> (ty, [], OCaml.unit)
  | Alpha, _ -> (ty, ["a"], printConstructor constr ["a"])
  | T, _ | AlphaT, _ -> (ty, ["t"], printConstructor constr ["t"])
  | Func1 (argTy, _), _ ->
    let singletonArg = genVarNames [argTy] in 
    (ty, singletonArg, printConstructor constr singletonArg)
  | Func2 (arg1, arg2, _), _ -> 
    let args = genVarNames [arg1; arg2] in 
    (ty, args, printConstructor constr args)
  

(** Generates the definition of the [gen_expr] Quickcheck generator for 
    the [expr] datatype *)  

(* TODO: replace [return Empty] with something more generic? *)
let genExprDef (m : moduleSig) : document = 
  let tyAssocList = tyADTConstructorsAssocList m in 
  
  (** Map each [ty] to the LHS of its corresponding pattern match *)
  let patternLHSAssocList : (ty * document) list = 
    tyAssocList
    |> List.map ~f:(fun (ty, c) -> (ty, OCaml.tuple [!^ c; underscore])) in

  (** Map each [ty] to the RHS of its corresponding pattern match *)
  let patternRHSAssocList : (ty, document) List.Assoc.t = 
    List.(tyAssocList 
    |> filter ~f:(fun (ty, _) -> tyIsArrow ty) 
    |> map ~f:getExprConstructor' 
    |> map ~f:genExprPatternRHS) in

  (** Given each (ty, patternLHS) pair, find the [patternRHS] that corresponds
      to the same ty *)
  let completePatterns : document list = 
    List.(map patternLHSAssocList ~f:(fun (ty, patternLHS) -> 
      let patternRHS = 
        begin match (Assoc.find patternRHSAssocList ~equal:tyEqual ty) with 
        | Some rhs -> rhs
        | None -> (!^ "failwith ") ^^ OCaml.string "Missing RHS for pattern"
        end in 
      hardline ^^ sBar ^^ 
      patternLHS ^^ sArrow ^^ jump 4 0 patternRHS)) in
  
  hardline
  ^^ hang 2 @@ 
  !^ "let rec gen_expr (ty : ty) : expr Generator.t = " 
  ^/^ (!^ "let module G = Generator in ")
  ^/^ (!^ "let open G.Let_syntax in ")
  ^/^ (!^ "let%bind k = G.size in ")
  ^/^ (!^ "match ty, k with ")
  ^/^ (sBar ^^ OCaml.tuple [!^ "T"; OCaml.int 0] ^^ sArrow ^^ (!^ "return Empty"))
  ^^ concat completePatterns
  
(* TODO: figure out which [expr]s return a certain [ty] *)




