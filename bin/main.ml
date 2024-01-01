module type S = sig 
  type abs_ty
end 
[@@deriving mod_expr]  




let () = Lib.entrypoint
