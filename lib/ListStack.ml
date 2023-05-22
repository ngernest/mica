open StackInterface

module ListStack : StackInterface = struct 
  type 'a t = 'a Base.List.t
    [@@deriving sexp]

  let empty = []

  let create () = empty

  let push x s = x :: s

  let peek s = 
    match s with 
    | [] -> failwith "can't peek from an empty stack"
    | x :: _ -> x
  
  let pop s = 
    match s with 
    | [] -> failwith "can't pop from an empty stack"
    | _ :: s' -> s'
  (* Manufacture a bug: by doing [| _ -> Some s] in the pattern match *)  

  let is_empty s = 
    match s with 
    | [] -> true 
    | _ -> false 

  let length s = List.length s

  let clear _ = ()
end 