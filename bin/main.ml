module type Set = sig 
  type 'a t
  val empty : 'a t
end
[@@deriving mica]  


(* 

Currently generated code: 
{[
  type expr = 
    | T of int 
]}

Want:
{[
  type expr = 
    | Empty
]}    

*)



let () = Lib.entrypoint
