open Lib.CmdLineParser

(** Launches the Mica executable, i.e. the command line parser *)
let () = Command_unix.run ~version:"1.1" cmdLineParser

(** Commented out code for basic testing *)
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
