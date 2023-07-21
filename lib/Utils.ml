open Base
open PPrint
open Stdio

(** This file contains general utility functions that are called 
    by the other files. *)

(** [writeDoc outc doc] writes the PPrint document [doc] to an Out_channel [doc] 
    (eg. [stdout]) *)
let writeDoc (outc : Out_channel.t) (doc : document) : unit = 
  ToChannel.pretty 1.0 60 outc doc

(** [spaceL doc] adds a space on the {b left} of the PPrint document [doc] *)  
let spaceL (doc : document) : document = doc ^^ space      

(** [spaceR doc] adds a space on the {b right} of the PPrint document [doc] *)    
let spaceR (doc : document) : document = doc ^^ space  

(** [spaceLR doc] adds a space on {b both} sides of the PPrint document [doc] *)  
let spaceLR (doc : document) : document = enclose space space doc  

(** Produces a PPrint document [" | "] with spaces on both sides *)  
let sBar : document = spaceLR bar  

(** Produces a PPrint document [ "| " ] with a space on the right only *)
let barSpace : document = spaceR bar

(** Produces a PPrint document [" -> "] with spaces on either side *)  
let sArrow : document = spaceLR (!^ "->")

(** Produces a PPrint document [ ** ] with spaces on either side *)  
let star2 : document = star ^^ star

(** Produces a PPrint document [" in "] with spaces on either side *)  
let sIn : document = spaceLR (!^ "in")

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

(** [parensStr s] parenthesizes the string [s] *)
let parensStr (s : string) : string = "(" ^ s ^ ")"
