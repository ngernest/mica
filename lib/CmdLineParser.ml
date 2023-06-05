open Core 
open PPrint
open Stdio

open Parser
open ParserTypes
open ModuleParser
open CodeGenerator
open Utils

(** Parses user input from the command-line, in particular the filepaths 
    of the [.ml] files containing the signature & the two modules for testing.
    (Code adapted from Real World OCaml, Chapter 16) *)

(** {1 Utility functions for command-line parsing} *)    

(** Prompts the user to enter a value, where [name] is the name of the value, 
    and [of_string] if a function converting the string input to type ['a] *)
let prompt_for_string (name : string) (of_string : string -> 'a) : 'a =
  printf "enter %s: %!" name;
  match In_channel.input_line In_channel.stdin with
  | None -> failwith "no value entered. aborting."
  | Some line -> of_string line

(** Creates a parser that automatically prompts the user with the message [name] 
    if a value isn’t provided, where [of_string] is used to convert the 
    user input from string to type ['a] *)  
let anon_prompt (name : string) (of_string : string -> 'a) : 'a Command.Param.t =
  let arg = Command.Arg_type.create of_string in
  let%map_open.Command value = anon (maybe (name %: arg)) in
  match value with
  | Some v -> v
  | None -> prompt_for_string name of_string

(** Checks if a string is a valid readable filename *)  
let is_filename (filename : string) : string =
  match Sys_unix.is_file filename with
  | `Yes -> filename
  | `No -> failwith "Not a regular file"
  | `Unknown ->
    failwith "Could not determine if this was a regular file"

(** Defines a cmd-line argument type [regular_file] which ensures that the 
    input file isn't some Unix file type that can’t be read *)      
let regular_file =
  Command.Arg_type.create is_filename
    
(** {1 Useful filepaths & flags} *)    

(** Name of the generated file containing the PBT code *)
let pbtFilePath : string = "./lib/Generated.ml"

(** Name of the generated executable file which compares two modules
    for observational equivalence *)
let execFilePath : string = "./bin/compare_impls.ml"

(** Name of a flag specifying whether QuickCheck integer generators 
    should only generate non-negative integers *)
let nonNegIntsFlag : string = "-non-negative-ints-only"

(** Documentation string for [nonNegIntsFlag] *)
let nonNegIntsDoc : string = 
  "Specify if QuickCheck int generators should only generate non-negative ints"

(** {1 Writing the generated PBT code to an output file} *)   

(** Writes the generated PBT code to the file at [pbtFilePath] 
    [m] is the parsed module signature AST 
    [functorName] is the name of the functor that produces the test harness
    [sigName, modName1, modName2] are the names of the signature/two module implementations *)
let writeToPBTFile (m : moduleSig) ~(pbtFilePath : string) ~(functorName : string)
  ~(sigName : string) ~(nonNegOnly: bool) (modName1 : string) (modName2 : string) : unit = 

  let pbtFile = Out_channel.create ~append:false pbtFilePath in
  writeDoc pbtFile 
    (imports sigName ~modName1 ~modName2 ^/^ exprADTDecl m ^/^ tyADTDecl m);
  writeDoc pbtFile (functorDef m ~sigName ~functorName);
  writeDoc pbtFile (genExprDef ~nonNegOnly m);
  writeDoc pbtFile (implModuleBindings ~functorName modName1 modName2);
  writeDoc pbtFile displayErrorDef;
  Out_channel.close pbtFile

(** {1 Parsing user input from the command-line} *)  
  
(** Parses the names of the signature & implementation files from the cmd-line *)
let cmdLineParser : Command.t =
  Command.basic
    ~summary:"Mica: Automated Property-Based Testing for OCaml modules"
    ~readme:(fun () -> 
      "For detailed documentation, please consult the docs at ngernest.github.io/module_pbt")
    (let%map_open.Command 
      sigFile = anon ("signature_file" %: regular_file) 
      and implFile1 = anon ("implementation_file_1" %: regular_file)
      and implFile2 = anon ("implementation_file_2" %: regular_file) 
      and nonNegOnly = flag nonNegIntsFlag no_arg ~doc:nonNegIntsDoc in
    fun () -> 
      let functorName = "ExprToImpl" in
      let moduleString = string_of_file sigFile in 
      let (sigName, modName1, modName2) = 
        map3 ~f:getModuleSigName (sigFile, implFile1, implFile2) in 
      begin match (run_parser moduleTypeP moduleString) with 
        | Ok m -> 
          writeToPBTFile m ~pbtFilePath ~functorName ~sigName ~nonNegOnly 
            modName1 modName2;

          (* TODO (stretch-goal): automatically append executable stanza to Dune file *)

          let executable = Out_channel.create ~append:false execFilePath in
            writeDoc executable (executableImports ~pbtFilePath ~execFilePath);
            writeDoc executable (compareImpls m);

            Out_channel.close executable;

        | Error err -> printf "error = %s\n" err
      end)


