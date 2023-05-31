open Base
open PPrint
open Stdio

(** This file contains general utility functions that are called 
    by the other files. *)

(** Writes a PPrint document to an Out_channel (eg. [stdout]) *)
let write_doc (outc : Out_channel.t) (doc : document) : unit = 
  ToChannel.pretty 1.0 60 outc doc

(** [spaced doc] adds a space on either side of the PPrint document [doc] *)  
let spaced (doc : document) : document = 
  enclose space space doc   

(** Produces a PPrint document [" | "] with spaces on both sides *)  
let sBar : document = spaced bar  

(** Produces a PPrint document [ "| " ] with a space on the right only *)
let barSpace : document = bar ^^ space

(** Produces a PPrint document [" -> "] with spaces on either side *)  
let sArrow : document = spaced (!^ "->")

(** Produces a PPrint document [ ** ] with spaces on either side *)  
let star2 : document = star ^^ star

(** Takes a PPrint document [body] and wraps it in the OCaml comment syntax, 
    i.e. [comment body] is displayed as [(** body *)] *)
let comment (body : document) : document = 
  parens @@ enclose (star2 ^^ space) (space ^^ star) body

(** Given a filepath to a .ml/.mli file, retrieves the corresponding name of the 
    top-level module signature. 
    - {b Note}: The name of the module signature must be the same as the .ml/.mli file. *)  
let getModuleSigName (filepath : string) : string =
  Core.Filename.(basename filepath |> chop_extension)

(** [replicate n a] produces a list containing [n] copies of [a] *)    
let replicate ~(n : int) (a : 'a) : 'a list = 
  let rec helper (n : int) (acc : 'a list) : 'a list = 
    if n = 0 then acc 
    else helper (n - 1) (a :: acc) in 
  helper n []  

(** Applies a function pointwise on a pair *)  
let map2 ~f (a1, a2) = (f a1, f a2)  

(** Applies a function pointwise on a triple *)  
let map3 ~f (a1, a2, a3) = (f a1, f a2, f a3)    

(** Converts a curried arity-3 function to its uncurried equivalent *)    
let curry3 f a b c = f (a, b, c)

(** Converts an uncurried arity-3 function to its curried equivalent *)
let uncurry3 f (a, b, c) = f a b c