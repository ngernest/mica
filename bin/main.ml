open Core
open PPrint
open Stdio

open Lib.Parser
open Lib.ParserTypes
open Lib.ModuleParser
open Lib.CodeGenerator
open Lib.CmdLineParser
open Lib.Utils

(** Name of the generated file containing the PBT code *)
let pbtFilePath : string = "./lib/Generated.ml"

(** Name of the generated executable file which compares two modules
    for observational equivalence *)
let execFilePath : string = "./bin/compare_impls.ml"

(* Disable "unused-values" compiler warnings *)
[@@@ocaml.warning "-32-34-27"]

(** Writes the generated PBT code to the file at [pbtFilePath] 
    [m] is the parsed module signature AST 
    [functorName] is the name of the functor that produces the test harness
    [sigName, modName1, modName2] are the names of the signature/two module implementations *)
let writeToPBTFile (m : moduleSig) ~(pbtFilePath : string) ~(functorName : string)
  (sigName : string) (modName1 : string) (modName2 : string) : unit = 

  let pbtFile = Out_channel.create ~append:false pbtFilePath in
  writeDoc pbtFile 
    (imports sigName ~modName1 ~modName2 ^/^ exprADTDecl m ^/^ tyADTDecl m);
  writeDoc pbtFile (functorDef m ~sigName ~functorName);
  writeDoc pbtFile (genExprDef m);
  writeDoc pbtFile (implModuleBindings ~functorName modName1 modName2);
  writeDoc pbtFile displayErrorDef;
  Out_channel.close pbtFile

(** Parses the names of the signature & implementation files from the cmd-line *)
let cmdLineParser : Command.t =
  Command.basic
    ~summary:"Automated Property-Based Testing for OCaml modules"
    ~readme:(fun () -> "TODO: Complete readme")
    (let%map_open.Command 
      sigFile = anon ("signature file" %: regular_file) 
      and implFile1 = anon ("implementation file 1" %: regular_file)
      and implFile2 = anon ("implementation file 2" %: regular_file) in
    fun () -> 
      let functorName = "ExprToImpl" in
      let moduleString = string_of_file sigFile in 
      let (sigName, modName1, modName2) = 
        map3 ~f:getModuleSigName (sigFile, implFile1, implFile2) in 
      begin match (run_parser moduleTypeP moduleString) with 
        | Ok m -> 
          writeToPBTFile m ~pbtFilePath ~functorName sigName modName1 modName2;

          (* TODO (stretch-goal): automatically append executable stanza to Dune file *)

          let executable = Out_channel.create ~append:false execFilePath in
            writeDoc executable (executableImports ~pbtFilePath ~execFilePath);
            writeDoc executable (compareImpls m);

            Out_channel.close executable;

        | Error err -> printf "error = %s\n" err
      end)


(* let testParser : Command.t =
  Command.basic
    ~summary:"Automated Property-Based Testing for OCaml modules"
    ~readme:(fun () -> "TODO: Complete readme")
    (let%map_open.Command 
      sigFile = anon ("signature file" %: regular_file) in
    fun () -> 
      let functorName = "ExprToImpl" in
      let moduleString = string_of_file sigFile in 
      let sigName = getModuleSigName sigFile in 
      begin match (run_parser moduleTypeP moduleString) with 
        | Ok m -> 
          writeToPBTFile m ~pbtFilePath ~functorName sigName "" ""

        | Error err -> printf "error = %s\n" err
      end)       *)

let () = Command_unix.run ~version:"1.1" cmdLineParser

  
