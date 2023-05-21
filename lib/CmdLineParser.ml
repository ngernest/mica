(** Code adapted from Real World OCaml, Chapter 16 *)
open Core 

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

(** Creates a parser that:
    (1) Defines a cmd-line argument type which ensures that the 
    input file isn't some Unix file type that can’t be read, and 
    (2) Prompts the user if no value is provided *)        
(** TODO: not sure why this doesn't work *)      
let anon_prompt_reg_file (name : string) : string Command.Param.t = 
  anon_prompt name is_filename

