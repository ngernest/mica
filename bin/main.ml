open! Core
open! Angstrom
open! Core_unix
open! PPrint
open! Stdio

open! Lib.Parser
open! Lib.ParserTypes
open! Lib.ModuleParser
open! Lib.CodeGenerator
open! Lib.CmdLineParser

(** Name of the generated file containing the PBT code *)
let pbtFilePath : string = "./lib/Generated_Poly.ml"

(** Name of the generated executable file which compares two modules
    for observational equivalence *)
let execFilePath : string = "./bin/compare_impls_poly.ml"

(* TODO: remove disabling of "unused-values" compiler warnings *)
[@@@ocaml.warning "-32-34-27"]

(** Writes the generated PBT code to the file at [pbtFilePath] 
    [m] is the parsed module signature AST 
    [functorName] is the name of the functor that produces the test harness
    [sigName, modName1, modName2] are the names of the signature/two module implementations *)
let writeToPBTFile (m : moduleSig) ~(pbtFilePath : string) ~(functorName : string)
  (sigName : string) (modName1 : string) (modName2 : string) : unit = 

  let pbtFile = Out_channel.create ~append:false pbtFilePath in
  write_doc pbtFile (imports sigName modName1 modName2 ^/^ exprADTDecl m ^/^ tyADTDecl m);
  write_doc pbtFile (functorDef m ~sigName ~functorName);
  write_doc pbtFile (genExprDef m);
  write_doc pbtFile (implModuleBindings ~functorName modName1 modName2);
  write_doc pbtFile displayErrorDef;
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
          let executable = Out_channel.create ~append:false execFilePath in
            write_doc executable (executableImports ~pbtFilePath ~execFilePath);
            write_doc executable (compareImpls m);

            Out_channel.close executable;

        | Error err -> printf "error = %s\n" err
      end)

let () = Command_unix.run ~version:"1.0" cmdLineParser
