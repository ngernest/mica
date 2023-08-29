open Lib.CmdLineParser

(** Launches the Mica executable, i.e. the command line parser *)
let () = Command_unix.run ~version:"1.1" cmdLineParser
