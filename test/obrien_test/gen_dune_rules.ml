open Core

(* Code generation for the testing process from:
    https://tarides.com/blog/2019-05-09-an-introduction-to-ocaml-ppx-ecosystem
*)

let ppx_name = "ppx_template"

module Test = struct
  type kind =
    | Passing
    | Errors

  let pp_executable ppf ~kind (name, modules) =
    match kind with
    | Passing ->
      Format.fprintf
        ppf
        "; The executable under test@,\
         @[<v 1>(executable@ (name %s)@ (modules %s)@ (preprocess (pps %s)))@]"
        name
        modules
        ppx_name
    | Errors ->
      (* If the PPX errors, then we don't declare the file as an executable 
         (since we don't want to compile it) *)
      ()
  ;;

  let pp_rule ppf ~kind (name, module_name) =
    let pp_action ppf () =
      match kind with
      | Errors ->
        Format.fprintf
          ppf
          "; expect the process to fail, capturing stderr@,\
           @[<v 1>(with-stderr-to@,\
           %%{targets}@,\
           (bash \"! ./%%{pp} -no-color --impl %%{input}\"))@]"
      | Passing ->
        Format.fprintf
          ppf
          "; expect the process to succeed, captured in target@,\
           (run ./%%{pp} --impl %%{input} -o %%{targets})"
    in
    Format.fprintf
      ppf
      "; Run the PPX on the `.ml` file@,\
       @[<v 1>(rule@,\
       (targets %s.actual)@,\
       @[<v 1>(deps@,\
       (:pp pp.exe)@,\
       (:input %s.ml))@]@,\
       @[<v 1>(action@,\
       %a))@]@]"
      name
      module_name
      pp_action
      ()
  ;;

  let pp_diff_alias_rule ppf name =
    Format.fprintf
      ppf
      "; Compare the post-processed output to the .expected file@,\
       @[<v 1>(rule@,\
       (alias runtest)@,\
       (package %s)@,\
       @[<v 1>(action@,\
       @[<hov 2>(diff@ %s.expected@ %s.actual)@])@])@]"
      ppx_name
      name
      name
  ;;

  let pp_run_alias_rule ppf ~kind module_name =
    match kind with
    | Passing ->
      (* If we expect the PPX expansion to succeed, then we should be able to compile the output. *)
      Format.fprintf
        ppf
        "@,\
         ; Ensure that the post-processed executable runs correctly@,\
         @[<v 1>(rule@,\
         (alias runtest)@,\
         (package %s)@,\
         @[<v 1>(action@,\
         @[<hov 2>(run@ ./%s.exe)@])@])@]"
        ppx_name
        module_name
    | Errors -> ()
  ;;

  let pp ppf ~kind filename =
    let name = Filename.chop_extension filename in
    let module_name = name in
    let modules =
      (* Each test should only consist of a single file (or module) *)
      module_name
    in
    Format.set_margin 80;
    Format.fprintf
      ppf
      "@[<v 0>; -------- Test: `%s.ml` --------%a@,@,%a@,@,%a%a@,@]@."
      module_name
      (pp_executable ~kind)
      (name, modules)
      (pp_rule ~kind)
      (name, module_name)
      pp_diff_alias_rule
      name
      (pp_run_alias_rule ~kind)
      module_name
  ;;

  module Suite = struct
    type t =
      { kind : kind
      ; files : string list
      }

    let is_test = function
      | "pp.ml" -> false
      | "gen_dune_rules.ml" -> false
      | filename ->
        Filename.check_suffix filename ".ml"
        (* Avoid capturing preprocessed files *)
        && not (Filename.check_suffix filename ".pp.ml")
    ;;

    let create ~kind files =
      let files = files |> List.sort ~compare:String.compare |> List.filter ~f:is_test in
      { kind; files }
    ;;

    let pp ppf { kind; files } = List.iter files ~f:(pp ppf ~kind)
  end
end

let command =
  let kind =
    Command.Arg_type.create (function
      | "passing" -> Test.Passing
      | "errors" -> Errors
      | kind -> Format.ksprintf failwith "Invalid kind: %s" kind)
  in
  Command.basic
    ~summary:(Format.sprintf "Generate dune rules for testing %s" ppx_name)
    (let%map_open.Command kind =
       flag
         "-kind"
         (optional_with_default Test.Passing kind)
         ~doc:"string Test suite kind"
     in
     fun () ->
       let test_suite = Test.Suite.create ~kind (Sys_unix.readdir "." |> Array.to_list) in
       Format.printf "%a\n" Test.Suite.pp test_suite)
;;

let () = Command_unix.run ~version:"1.0" command
