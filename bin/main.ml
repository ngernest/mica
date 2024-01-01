module type S = sig 
  type 'a t
end
[@@deriving mica]  




let () = Lib.entrypoint
